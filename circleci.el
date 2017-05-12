;;; circleci.el --- Get CI build output in Emacs

;; Copyright © 2017 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/circleci.el
;; Version: 0.1.0
;; Created: 2017-05-12
;; Keywords: convenience tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Gives you a buffer with a list build steps for any given
;; build. Press enter on a step to expand.

;; It will prompt you for a CircleCI token and can save that token
;; in ~/.authinfo.gpg if you have your GPG key set up properly.

;; You can create a CircleCI token at https://circleci.com/account/api

;; Note that this is not officially supported by CircleCI.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl)
(require 'url)
(require 'json)

(defvar circleci-url "https://circleci.com/api/v1/project/")

;;;###autoload
(defun circleci (build)
  "Show build output for a given CircleCI build."
  (interactive "MBuild: (org/project/buildnum) ") ; TODO: guess at build number?
  (let* ((auth-sources '("~/.authinfo.gpg"))
         (secret-spec (car (auth-source-search :host "circleci.com" :user "none"
                                               :create t :type 'netrc)))
         (token-fn (plist-get secret-spec :secret))
         (save-fn (plist-get secret-spec :save-function))
         (url-request-extra-headers '(("Accept" . "application/json"))))
    (when save-fn (funcall save-fn))
    (url-retrieve (concat circleci-url build"?circle-token=" (funcall token-fn))
                  'circleci-process-build)))

(defun circleci-process-build (status)
  "Callback for overall build response."
  (goto-char (point-min))
  (search-forward "\n\n")
  (delete-region (point-min) (point))
  (let* ((json-array-type 'list)
         (steps (cdr (assoc 'steps (json-read-object))))
         (output '()))
    (switch-to-buffer "*circleci-output*")
    (local-set-key (kbd "q") 'bury-buffer)
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (dolist (step steps)
        (circleci-add-step step)))
    (setq buffer-read-only t)))

(defun circleci-add-step (step)
  (let ((actions (car (cdr (assoc 'actions step)))))
    (insert-text-button (format "# %s" (cdr (assoc 'name actions)))
                        'action (apply-partially 'circleci-step-action
                                                 step (point)))
    (insert "\n\n")))

(defun circleci-step-action (step point button)
  (let ((actions (car (cdr (assoc 'actions step)))))
    (when (and (cdr (assoc 'has_output actions))
               (cdr (assoc 'output_url actions)))
      (message "Retreiving %s" (cdr (assoc 'name actions)))
      (let (buffer-read-only) ; button disables itself
        (button-put button 'action 'ignore))
      (url-retrieve (cdr (assoc 'output_url actions))
                    (apply-partially 'circleci-decompress-step point)))))

(defun circleci-decompress-step (point status)
  "Callback for fetching output for a specific step."
  (let ((filename (make-temp-file "download" nil ".gz"))
        (json-array-type 'list))
    (search-forward "\n\n")
    (write-region (point) (point-max) filename)
    (with-auto-compression-mode
      (find-file filename)
      (dolist (msg (json-read))
        (when (string= "out" (cdr (assoc 'type msg)))
          (with-current-buffer "*circleci-output*"
            (let (buffer-read-only)
              (goto-char point)
              (end-of-line)
              (insert "\n\n")
              ;; TODO: replace ^M with newlines
              (insert (cdr (assoc 'message msg)))))))
      (kill-buffer))))

(provide 'circleci)
;;; circleci.el ends here
