(in-package #:robots.txt)

(defun last-char-= (char string)
  (char= char (char string (1- (length string)))))
(declaim (inline last-char-=))

(defclass wild-uri ()
  ((raw-uri :initarg :raw)
   (regexp)
   (scanner)))

(defmethod initialize-instance :after ((object wild-uri) &key raw &allow-other-keys)
  (let* ((anchor-end (last-char-= #\$ raw))
         (split (split "([*])" (subseq raw 0 (when anchor-end (- (length raw) 2) (1- (length raw)))) :with-registers-p t))
         (join (mapcar (lambda (pattern) (if (string= pattern "*")
                                             `(:non-greedy-repetition 0 nil :everything)
                                             pattern))
                       split)))
    (setf (slot-value object 'regexp)
          `(:sequence :start-anchor ,@join ,@(when anchor-end '(:end-anchor))))
    (setf (slot-value object 'scanner)
          (create-scanner (slot-value object 'regexp)))))

#+(or) (make-instance 'wild-uri :raw "/foo*/bar/baz")
#+(or) (make-instance 'wild-uri :raw "/foo/bar/baz$")

(defun wild-uri-p (uri)
  (or (find #\* uri) (last-char-= #\$ uri)))
(declaim (inline wild-uri-p))
