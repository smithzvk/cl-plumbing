
(in-package :cl-plumbing)

(defun dump-streams (input output)
  (with-open-stream (echo (make-echo-stream input output))
    (iter (for c = (read-char echo nil nil))
      (while c))))

(defvar *warn-on-foreground-connect-stream* t)

(defun connect-streams (input output &optional (background t))
  "This reads from input and writes output until the end of input is found."
  #-bordeaux-threads
  (progn
    (when (and background *warn-on-foreground-connect-stream*)
      (warn "Unable to run in background without multithreading support"))
    (dump-streams input output))
  #+bordeaux-threads
  (if background
      (bt:make-thread
       (lambda ()
         (dump-streams input output)))
      (dump-streams input output)))

