
(defpackage :cl-plumbing-test
  (:use :cl :cl-plumbing :stefil :iterate)
  (:export))

(in-package :cl-plumbing-test)

(in-root-suite)

(deftest iterate-test ()
  "Test to see if the pipes work with Iterates in-stream driver."
  (let ((pipe (make-pipe)))
    (print 1 pipe)
    (print 2 pipe)
    (print 3 pipe)
    ;; have to close input before bulk read, otherwise hangs forever:
    (close pipe)
    (is (equal '(1 2 3)
               (iter (for val in-stream pipe)
                     (collect val))))))

(deftest pipe-test ()
  (let ((input "hello howdy heck"))
    (let ((pipe (make-pipe)))
      (iter (for c in-sequence input)
            (write-char c pipe)
            (is (equal c (read-char pipe)))))
    (is (equal input
               (let ((pipe (make-pipe)))
                 (iter (for c in-sequence input)
                       (write-char c pipe))
                 (close pipe) ; have to close before bulk read
                 (iter (for c = (read-char pipe nil nil))
                       (while c)
                       (collect c result-type 'string)))))))
