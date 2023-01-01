(in-package :asdf-user)
(defsystem "cl-cryptogrammer-tests"
  :description "Test suite for the cl-cryptogrammer system"
  :author "gregoryg <gregory@dynapse.com>"
  :version "0.0.1"
  :depends-on (:cl-cryptogrammer
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-cl-cryptogrammer"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
