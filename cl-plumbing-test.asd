
(asdf:defsystem #:cl-plumbing-test
  :name "cl-plumbing-test"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :description
  "Tests for a few (at least seemingly) missing stream operations in Common
Lisp."
  :components ((:file "test"))
  :serial t
  :depends-on (:iterate :cl-plumbing :stefil))

