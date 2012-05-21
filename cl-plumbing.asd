
(asdf:defsystem #:cl-plumbing
  :name "CL-Plumbing"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :description "A few (at least seemingly) missing stream operations in Common Lisp."
  :components ((:file "package")
               #+(or abcl ccl clisp cmu ecl sbcl allegro lispworks)
               (:file "gray-streams-pipe")
               #-(or abcl ccl clisp cmu ecl sbcl allegro lispworks)
               (:file "fifo-pipe")
               (:file "cl-plumbing"))
  :serial t
  :depends-on (:iterate :trivial-gray-streams :bordeaux-threads))
