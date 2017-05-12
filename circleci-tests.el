(ert-deftest run-on-self ()
  (should (equal 2 (+ 1 1)))
  ;; (cl-letf ((auth-source-search (&rest _)
  ;;                               `((:secret ,(getenv "CIRCLECI_TOKEN")
  ;;                                          :save-function ignore))))
  ;;   ;; prefix arg set to 1 means infer project/branch
  ;;   (circleci 1))
  )


