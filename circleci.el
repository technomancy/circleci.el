;;; -*- lexical-binding: t -*-
;;; circleci.el --- Get CI build output in Emacs

;; Copyright © 2017 Phil Hagelberg and contributors

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/circleci.el
;; Version: 0.2.2
;; Created: 2017-05-12
;; Keywords: convenience tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Gives you a buffer with a list builds steps for any given project.
;; Press enter on a build to see steps, or press g to refresh.  Use
;; C-u M-x circleci to have it prompt for a specific project and
;; branch instead of infering from your .git directory. M-x
;; circleci-latest takes you directly to the latest build for a
;; project.

;; It will prompt you for a CircleCI token and can save that token
;; in ~/.authinfo.gpg if you have your GPG key set up properly.

;; You can create a CircleCI token at https://circleci.com/account/api

;; Note that this is not officially supported by CircleCI.

;; To install, place it on your load-path and add an autoload:

;; (add-to-list 'load-path "~/src/circleci.el")
;; (autoload 'circleci "circleci" "Show CI build output" t)

;; NB. Visiting a specific build from the list doesn't work on 1.0 builds.

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

(defvar circleci-list-branch-url
  "https://circleci.com/api/v1.1/project/github/%s/tree/%s?circle-token=%s")
(defvar circleci-build-url
  "https://circleci.com/api/v1/project/%s/%s?circle-token=%s")

(defvar circleci-last-project nil)

(defun circleci--get-token ()
  (let* ((auth-sources '("~/.authinfo.gpg"))
         (secret-spec (car (auth-source-search :host "circleci.com" :user "none"
                                               :create t :type 'netrc)))
         (token (funcall (plist-get secret-spec :secret)))
         (save-fn (plist-get secret-spec :save-function)))
    (when save-fn (funcall save-fn))
    token))

(defun circleci--read-project ()
  (read-from-minibuffer "Organization/project: " nil nil nil nil
                        circleci-last-project))

(defun circleci--get-project ()
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert (shell-command-to-string "git remote show origin"))
          (goto-char (point-min))
          (search-forward-regexp "github.com[:/]\\(.+?\\)\\(\\.git\\)")
          (match-string 1))
      (error (circleci-read-project)))))

(defun circleci--request-for-project-and-branch (query? url callback)
  (let* ((token (circleci--get-token))
         (url-request-extra-headers '(("Accept" . "application/json")))
         (project (if query?
                      (or circleci-last-project
                          (circleci--get-project))
                    (circleci--read-project)))
         (branch (if query?
                     (car (vc-git-branches))
                   (completing-read "Branch: " (vc-git-branches))))
         (url (format url project branch token)))
    (setq circleci-last-project project)
    (url-retrieve url (apply-partially callback url project token))))

;;;###autoload
(defun circleci-latest (query)
  "Show build output for the latest CircleCI build of the current branch.

Call with prefix arg to prompt for project and branch interactively."
  (interactive "p")
  (circleci--request-for-project-and-branch (= 1 query)
                                            circleci-list-branch-url
                                            'circleci--process-builds))

;;;###autoload
(defun circleci (query)
  "Show list of builds for the CircleCI project.

Call with prefix arg to prompt for project and branch interactively."
  (interactive "p")
  (circleci--request-for-project-and-branch (= 1 query)
                                            circleci-list-branch-url
                                            'circleci--show-builds))

(defun circleci-refresh-build-list ()
  "Refresh the list of builds."
  (interactive)
  (url-retrieve circleci-this-build-url
                (apply-partially 'circleci--show-builds
                                 circleci-this-build-url
                                 circleci-project
                                 (circleci--get-token))))

(defun circleci--show-builds (url project token _status)
  "Callback for build listing response."
  (goto-char (point-min))
  (when (not (search-forward "HTTP/1.1 200" nil t))
    ;; pull HTTP status out of response
    (error "Problem fetching builds: %s" (buffer-substring 10 13)))
  (search-forward "\n\n")
  (let* ((json-array-type 'list)
        (builds (json-read)))
    (switch-to-buffer (format "*circleci-builds: %s*" project))
    (local-set-key (kbd "q") 'bury-buffer)
    (local-set-key (kbd "g") 'circleci-refresh-build-list)
    (set (make-local-variable 'circleci-project) project)
    (set (make-local-variable 'circleci-this-build-url) url)
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (insert (propertize (concat "Builds for " project "\n\n")
                          'font-lock-face '(:weight bold)))
      (dolist (build builds)
        (circleci--insert-build project build token)))
    (goto-char (point-min))
    (font-lock-mode 1)
    (setq buffer-read-only t)))

(defvar circleci--status-colors '(("success" . "green")
                                  ("fixed" . "green")
                                  ("failed" . "red")
                                  ("running" . "blue")
                                  ("not_run" . "grey")
                                  ("queued" . "purple")))

(defun circleci--colored-status (status)
  (let ((color (cdr (assoc status circleci--status-colors))))
    (if color
        (propertize status 'font-lock-face (list :foreground color))
      (or status "unknown-status"))))

(defun circleci--insert-build (project build token)
  (let* ((job-name (or (cdr (assoc 'job_name (cdr (assoc 'workflows build))))
                       (cdr (assoc 'job_name build))))
         (build-num (cdr (assoc 'build_num build)))
         (subject (cdr (assoc 'subject build)))
         (status (cdr (assoc 'status build)))
         (sha (cdr (assoc 'vcs_revision build)))
         (sha (substring (or sha "unknown?") 0 8))
         (compare-url (cdr (assoc 'compare build)))
         (build-url (cdr (assoc 'build_url build)))
         (num (if job-name
                  (format "#%s %s" build-num job-name)
                (format "#%s" build-num))))
    (insert-text-button num 'action (apply-partially 'circleci--build-action
                                                     project build-num token))
    (insert " " (circleci--colored-status status) " ")
    (insert-text-button "⥀" 'action (apply-partially 'browse-url build-url))
    (insert "\n")
    (insert-text-button sha 'action (apply-partially 'browse-url compare-url))
    (insert (format " %s\n\n" (or subject "unknown commit message")))))

(defun circleci--build-action (project build-num token button)
  (url-retrieve (format circleci-build-url project build-num token)
                (apply-partially 'circleci-process-build project)))

(defun circleci--process-builds (_url project token _status)
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

;;; handling a single build

(defun circleci-process-build (project status)
  "Callback for overall individual build response."
  (goto-char (point-min))
  (when (not (search-forward "HTTP/1.1 200" nil t))
    ;; pull HTTP status out of response
    (error "Problem fetching build: %s" (buffer-substring 10 13)))
  (search-forward "\n\n")
  (let* ((json-array-type 'list)
         (build (json-read))
         (steps (cdr (assoc 'steps build))))
    (switch-to-buffer (format "*circleci-output: %s*" project))
    (local-set-key (kbd "q") 'bury-buffer)
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (circleci--insert-build project build (circleci--get-token))
      (if (null steps)
          (insert "No steps for this build.")
        (dolist (step steps)
          (circleci-add-step project step))))
    (goto-char (point-min))
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
        (json-array-type 'list)
        (recentf-keep (list (lambda (_) nil))))
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
              ;; TODO: should call ansi-color-apply here, but it truncates
              ;; the string and throws data away
              (insert (cdr (assoc 'message msg)))
              (save-excursion
                (goto-char (point-min))
                (replace-string "" ""))))))
      (kill-buffer))))

(provide 'circleci)
;;; circleci.el ends here
