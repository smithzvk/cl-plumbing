
(in-package :cl-plumbing)

(defun dump-stream (input output &optional fn)
  (with-open-stream (echo (make-echo-stream input output))
    (iter (for c = (read-char echo nil nil))
      (when fn (funcall fn c))
      (while c))))

(defvar *warn-on-foreground-connect-stream* t)

(defun connect-streams (input output &key (background t) fn)
  "This reads from input and writes output until the end of input is found."
  #-bordeaux-threads
  (progn
    (when (and background *warn-on-foreground-connect-stream*)
      (warn "Unable to run in background without multithreading support"))
    (dump-stream input output fn))
  #+bordeaux-threads
  (if background
      (bt:make-thread
       (lambda ()
         (dump-stream input output fn)))
      (dump-stream input output fn)))
