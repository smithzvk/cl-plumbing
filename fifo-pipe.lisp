
(in-package :cl-plumbing)

;; This is annoying, but in certain lisps (the ones that uses buffered output)
;; you need to make sure that the output is flushed because otherwise the read
;; will not see anything and will hang.  One reason that this bites us here much
;; more than usual is because the stream is still open even after we are done
;; with the output.  This is because, in part, I use a :io stream.

;; However, this does in fact work, if you finess it.  See the example:

;; (with-open-stream (pipe (make-pipe))
;;   (let ((*standard-output* pipe))
;;     (print 'hello)
;;     (force-output))
;;   (print (read pipe)))

(defun make-pipe ()
  "This makes a stream where you can write your output, then read it out
elsewhere."
  (let ((random-fifo (format nil "/tmp/cl-plumbing-tmp-pipe-~A-~A"
                             (get-universal-time) (random 1000000))))
    #>(mkfifo ,random-fifo)
    (let ((str (open (pathname random-fifo) :direction :io :if-exists :overwrite)))
      (trivial-garbage:finalize
       str
       (lambda () #>(rm -f ,random-fifo)))
      str)))
