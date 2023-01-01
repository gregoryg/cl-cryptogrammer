
(load "cl-cryptogrammer.asd")
(load "cl-cryptogrammer-tests.asd")

(ql:quickload "cl-cryptogrammer-tests")

(in-package :cl-cryptogrammer-tests)

(uiop:quit (if (run-all-tests) 0 1))
