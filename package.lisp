
(defpackage :cl-plumbing
  (:use :cl :iterate)
  (:export
   #:connect-streams
   #:*warn-on-foreground-connect-stream*
   #:make-pipe))
