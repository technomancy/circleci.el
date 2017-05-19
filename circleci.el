;;; -*- lexical-binding: t -*-
;;; circleci.el --- Get CI build output in Emacs

;; Copyright © 2017 Phil Hagelberg and contributors

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/circleci.el
;; Version: 0.1.0
;; Created: 2017-05-12
;; Keywords: convenience tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Gives you a buffer with a list build steps for any given
;; build. Press enter on a step to expand. Use C-u M-x circleci to
;; have it prompt for a specific project and branch instead of
;; infering from your .git directory.

;; It will prompt you for a CircleCI token and can save that token
;; in ~/.authinfo.gpg if you have your GPG key set up properly.

;; You can create a CircleCI token at https://circleci.com/account/api

;; Note that this is not officially supported by CircleCI.

;; To install, place it on your load-path and add an autoload:

;; (add-to-list 'load-path "~/src/circleci.el")
;; (autoload 'circleci "circleci" "Show CI build output" t)

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
(require 'vc-git)

(defvar circleci-list-url
  "https://circleci.com/api/v1.1/project/github/%s/tree/%s?circle-token=%s")
(defvar circleci-build-url
  "https://circleci.com/api/v1/project/%s/%s?circle-token=%s")

(defvar circleci-last-project nil)

;;;###autoload
(defun circleci (query?)
  "Show build output for the latest CircleCI build of the current branch.

Call with prefix arg to prompt for project and branch interactively."
  (interactive "p")
  (let* ((auth-sources '("~/.authinfo.gpg"))
         (secret-spec (car (auth-source-search :host "circleci.com" :user "none"
                                               :create t :type 'netrc)))
         (token (funcall (plist-get secret-spec :secret)))
         (save-fn (plist-get secret-spec :save-function)))
    (when save-fn (funcall save-fn))
    (let ((url-request-extra-headers '(("Accept" . "application/json")))
          (project (if (= 1 query?)
                       (circleci-get-project)
                     (circleci-read-project)))
          (branch (if (= 1 query?)
                      (car (vc-git-branches))
                    (read-from-minibuffer "Branch: " nil nil nil nil
                                          (car (vc-git-branches))))))
      (setq circleci-last-project project)
      (url-retrieve (format circleci-list-url project branch token)
                    (apply-partially 'circleci-process-builds project token)))))

(defun circleci-read-project ()
  (read-from-minibuffer "Organization/project: " nil nil nil nil
                        circleci-last-project))

(defun circleci-get-project ()
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert (shell-command-to-string "git remote show origin"))
          (goto-char (point-min))
          (search-forward-regexp "github.com[:/]\\(.+?\\)\\(\\.git\\)")
          (match-string 1))
      (error (circleci-read-project)))))

(defun circleci-process-builds (project token status)
  "Callback for build listing response."
  (goto-char (point-min))
  (when (not (search-forward "HTTP/1.1 200" nil t))
    ;; pull HTTP status out of response
    (error "Problem fetching builds: %s" (buffer-substring 10 13)))
  (search-forward "\n\n")
  (let* ((json-array-type 'list)
         (build (first (json-read)))
         (build-num (cdr (assoc 'build_num build))))
    (message "Retrieving build #%s" build-num)
    (url-retrieve (format circleci-build-url project build-num token)
                  (apply-partially 'circleci-process-build project))))

(defun circleci-process-build (project status)
  "Callback for overall individual build response."
  (goto-char (point-min))
  (when (not (search-forward "HTTP/1.1 200" nil t))
    ;; pull HTTP status out of response
    (error "Problem fetching build: %s" (buffer-substring 10 13)))
  (search-forward "\n\n")
  (let* ((json-array-type 'list)
         (steps (cdr (assoc 'steps (json-read)))))
    (switch-to-buffer (format "*circleci-output: %s*" project))
    (local-set-key (kbd "q") 'bury-buffer)
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (if (null steps)
          (insert "No steps for this build.")
        (dolist (step steps)
          (circleci-add-step project step))))
    (setq buffer-read-only t)))

(defun circleci-add-step (project step)
  (let ((actions (car (cdr (assoc 'actions step)))))
    (insert-text-button (format "# %s" (cdr (assoc 'name actions))) 'action
                        (apply-partially 'circleci-step-action project step))
    (insert "\n\n")))

(defun circleci-step-action (project step button)
  (let ((actions (car (cdr (assoc 'actions step)))))
    (when (and (cdr (assoc 'has_output actions))
               (cdr (assoc 'output_url actions)))
      (message "Retreiving %s" (cdr (assoc 'name actions)))
      (let (buffer-read-only) ; button disables itself
        (button-put button 'action 'ignore))
      (url-retrieve (cdr (assoc 'output_url actions))
                    (apply-partially 'circleci-decompress-step project (point))))))

(defun circleci-decompress-step (project point status)
  "Callback for fetching output for a specific step."
  (let ((filename (make-temp-file "download" nil ".gz"))
        (json-array-type 'list))
    (search-forward "\n\n")
    (write-region (point) (point-max) filename)
    (with-auto-compression-mode
      (find-file filename)
      (dolist (msg (json-read))
        (when (string= "out" (cdr (assoc 'type msg)))
          (with-current-buffer (format "*circleci-output: %s*" project)
            (let (buffer-read-only)
              (goto-char point)
              (end-of-line)
              (insert "\n\n")
              (insert (cdr (assoc 'message msg)))
              (save-excursion
                (goto-char (point-min))
                (replace-string "" ""))))))
      (kill-buffer))))

(provide 'circleci)
;;; circleci.el ends here
