(in-package :asdf-user)

(defsystem "cl-cryptogrammer"
  :author "gregoryg <gregory@dynapse.com>"
  :version "0.0.1"
  :license "MIT"
  :description "Generate cryptograms."
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on (:alexandria
               :cl-dbi
               :dexador
               :fset
               :jonathan
               :str)

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "cl-cryptogrammer"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "cl-cryptogrammer"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "cl-cryptogrammer:main")
