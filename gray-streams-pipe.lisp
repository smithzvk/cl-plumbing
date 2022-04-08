(in-package :cl-plumbing)

;; Gray Stream version

(defclass pipe (trivial-gray-streams:fundamental-input-stream
                trivial-gray-streams:fundamental-output-stream
                trivial-gray-streams:trivial-gray-stream-mixin)
  ((lock :initform (bordeaux-threads:make-lock) :accessor lock-of)
   (cvar :initform (bordeaux-threads:make-condition-variable) :accessor cvar-of)
   (input :initarg :input :accessor input-of)
   (output :initarg :output :accessor output-of)
   (element-type :initarg :element-type :accessor element-type)))

;; Macro automating condition-notify
(defmacro with-condition-notify (pipe &body body)
  "Use to automatically notify read functions that the write function
has completed."
  (alexandria:with-gensyms (p cvar)
    `(let* ((,p ,pipe))
       (with-accessors ((,cvar cvar-of)) ,p
         (progn ,@body)
         (bordeaux-threads:condition-notify ,cvar)))))

(defmacro with-condition-wait (pipe &body body)
  "Use to automatically wait on write functions to finish before reading."
  (alexandria:with-gensyms (p lock cvar)
    `(let* ((,p ,pipe))
       (with-accessors ((,lock lock-of)
                        (,cvar cvar-of))
           ,p
         (progn ,@body)
         (bordeaux-threads:condition-wait ,cvar ,lock)))))

(defmethod initialize-instance :after
    ((p pipe) &key)
  (setf (element-type p)
        (stream-element-type (output-of p))))

(defmethod stream-element-type ((stream pipe))
  (element-type stream))

(defmethod trivial-gray-streams:stream-write-char ((p pipe) character)
  (with-condition-notify p
    (bt:with-lock-held ((lock-of p))
      (write-char character (output-of p)))))

(defmethod trivial-gray-streams:stream-write-string
    ((p pipe) string &optional start end)
  (with-condition-notify p
    (let* ((str (subseq string (if start start 0) end)))
      (bt:with-lock-held ((lock-of p))
        (write-string str (output-of p))))))

(defun flush-in-to-out (pipe)
  (let ((string (get-output-stream-string (output-of pipe))))
    (when (> (length string) 0)
      (setf (input-of pipe)
            (make-concatenated-stream
             (input-of pipe)
             (make-string-input-stream string))))))

(defmethod trivial-gray-streams:stream-read-char ((p pipe))
  (iter
    (with-condition-wait p
      (bt:with-lock-held ((lock-of p))
        (let ((eof (not (open-stream-p (output-of p)))))
          (flush-in-to-out p)
          (let ((result (read-char (input-of p) nil :eof)))
            (cond ((not (equal :eof result)) (return result))
                  ((and eof (equal :eof result)) (return :eof))
                  (t nil))))))))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((p pipe))
  (block nil
    (bt:with-lock-held ((lock-of p))
      (let ((eof (not (open-stream-p (output-of p)))))
        (flush-in-to-out p)
        (let ((result (read-char (input-of p) nil :eof)))
          (cond ((not (equal :eof result)) (return result))
                ((and eof (equal :eof result)) (return :eof))
                (t nil)))))))

(defmethod trivial-gray-streams:stream-unread-char ((p pipe) character)
  (bt:with-lock-held ((lock-of p))
    (unread-char character (input-of p))))

(defparameter *block-size* 1024)

(defmethod trivial-gray-streams:stream-read-line ((p pipe))
  (let ((consumed nil))
    (unwind-protect
         (iter
           (bt:with-lock-held ((lock-of p))
             (flush-in-to-out p)
             (let* ((eof (not (open-stream-p (output-of p))))
                    (seq (make-array (list *block-size*)))
                    (n-read (read-sequence seq (input-of p)))
                    (newline-marker (iter (for char in-sequence seq with-index i)
                                          (while (< i n-read))
                                          (finding i such-that (eql char #\Newline)))))
               (cond ((and newline-marker (< newline-marker n-read))
                      (setf (input-of p) (make-concatenated-stream
                                          (make-string-input-stream
                                           (coerce (subseq seq (+ newline-marker 1) n-read)
                                                   'string))
                                          (input-of p)))
                      (let ((c consumed))
                        (setf consumed nil)
                        (return (coerce (apply #'concatenate
                                               'string
                                               (reverse
                                                (cons
                                                 (subseq seq 0 newline-marker)
                                                 c)))
                                        'string))))
                     (eof (let ((c consumed))
                            (setf consumed nil)
                            (return
                              (values (coerce (apply #'concatenate
                                                     'string
                                                     (reverse
                                                      (cons
                                                       (subseq seq 0 n-read)
                                                       c)))
                                              'string) t))))
                     (t (push (subseq seq 0 n-read) consumed)))))
           ;; Block until there is more to read.
           (unread-char (read-char p) p))
      (setf (input-of p)
            (apply
             'make-concatenated-stream
             (reverse
              (cons (input-of p)
                    (mapcar
                     (lambda (x) (make-string-input-stream (coerce x 'string)))
                     consumed))))))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((p pipe) seq start end &key &allow-other-keys)
  (bt:with-lock-held ((lock-of p))
    (flush-in-to-out p)
    (read-sequence seq (input-of p) :start start :end end)))

(defmethod trivial-gray-streams:stream-write-sequence
    ((p pipe) seq start end &key &allow-other-keys)
  (with-condition-notify p
    (bt:with-lock-held ((lock-of p))
      (write-sequence seq (output-of p) :start start :end end))))

(defmethod trivial-gray-streams:stream-line-column ((p pipe))
  0)

(defmethod close ((p pipe) &key abort)
  (declare (ignore abort))
  (close (output-of p)))

(defun make-pipe ()
  "This makes a stream where you can write your output, then read it out
elsewhere."
  (make-instance 'pipe
                 :input (make-string-input-stream "")
                 :output (make-string-output-stream)))
