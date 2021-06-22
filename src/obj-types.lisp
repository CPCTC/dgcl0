;;;; Specifies which object types correspond to which characters.

(in-package :dgcl0-int)

(defvar *object-type-characters*
  (let ((table (make-hash-table)))

    (setf (gethash 'unknown table) #\.)
    (setf (gethash 'sense table) #\.)
    (setf (gethash 'shoot table) #\.)
    (setf (gethash 'translate table) #\.)
    (setf (gethash 'rotate table) #\.)
    (setf (gethash 'connect-disconnect table) #\.)
    (setf (gethash 'mimic table) #\.)

    (setf (gethash 'bullet table) #\*)

    table))

(defmacro obj-type-char (c)
  `(values (gethash ,c *object-type-characters*)))
