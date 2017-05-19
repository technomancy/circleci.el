(ert-deftest run-on-self ()
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _)
               `((:secret ,(apply-partially 'getenv "CIRCLECI_TOKEN")
                          :save-function ignore)))))
    (circleci 1) ; prefix arg set to 1 means infer project/branch
    (sit-for 4)
    (with-current-buffer (format "*circleci-output: %s*" circleci-last-project)
      (goto-char (point-min))
      (search-forward "ert-run-tests-batch-and-exit")
      (backward-char)
      (push-button)
      (sit-for 4)
      (goto-char (point-min))
      (search-forward "run-on-self"))))