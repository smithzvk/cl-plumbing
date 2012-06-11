(in-package :cl-plumbing)

;; Gray Stream version

(defclass pipe (trivial-gray-streams:fundamental-stream
                trivial-gray-streams:trivial-gray-stream-mixin)
  ((lock :initform (bordeaux-threads:make-lock) :accessor lock-of)
   (input :initarg :input :accessor input-of)
   (output :initarg :output :accessor output-of)))

;; (defmethod stream-element-type ((stream pipe))
;;   (stream-element-type (output-of stream)))

(defmethod trivial-gray-streams:stream-write-char ((p pipe) character)
  (bt:with-lock-held ((lock-of p))
    (write-char character (output-of p))))

(defun flush-in-to-out (pipe)
  (let ((string (get-output-stream-string (output-of pipe))))
    (when (> (length string) 0)
      (setf (input-of pipe)
            (make-concatenated-stream
             (input-of pipe)
             (make-string-input-stream string))))))

(defmethod trivial-gray-streams:stream-read-char ((p pipe))
  (bt:with-lock-held ((lock-of p))
    (flush-in-to-out p)
    (read-char (input-of p) nil :eof)))

(defmethod trivial-gray-streams:stream-unread-char ((p pipe) character)
  (bt:with-lock-held ((lock-of p))
    (unread-char character (input-of p))))

(defmethod trivial-gray-streams:stream-read-line ((p pipe))
  (bt:with-lock-held ((lock-of p))
    (flush-in-to-out p)
    (read-line (input-of p) nil :eof)))

(defmethod trivial-gray-streams:stream-read-sequence
    ((p pipe) seq start end &key &allow-other-keys)
  (bt:with-lock-held ((lock-of p))
    (flush-in-to-out p)
    (read-sequence seq (input-of p) :start start :end end)))

(defmethod trivial-gray-streams:stream-write-sequence
    ((p pipe) seq start end &key &allow-other-keys)
  (bt:with-lock-held ((lock-of p))
    (write-sequence seq (output-of p) :start start :end end)))

(defmethod trivial-gray-streams:stream-line-column ((p pipe))
  0)

(defun make-pipe ()
  "This makes a stream where you can write your output, then read it out
elsewhere."
  (make-instance 'pipe
                 :input (make-string-input-stream "")
                 :output (make-string-output-stream)))
