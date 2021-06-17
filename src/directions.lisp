;;; Provides canonical-dir, a way to convert
;;; various user-friendly direction specifiers
;;; into consistant machine-readable directions.

(in-package :dgcl0-int)

(defgeneric canonical-dir (dir))

(defmethod canonical-dir ((dir integer))
  (if (<= 0 dir 3)
    dir
    (error "Invalid direction ~a." dir)))

(defvar *valid-direction-strings*
  (let ((table (make-hash-table :test #'equalp)))

    (setf (gethash "right" table) 0)
    (setf (gethash "up"    table) 1)
    (setf (gethash "left"  table) 2)
    (setf (gethash "down"  table) 3)

    (setf (gethash "east"  table) 0)
    (setf (gethash "north" table) 1)
    (setf (gethash "west"  table) 2)
    (setf (gethash "south" table) 3)

    table))

(defmethod canonical-dir ((dir string))
  (let ((cdir
          (gethash dir *valid-direction-strings*)))
    (if cdir
      cdir
      (error "Invalid direction ~a." dir))))

(defmethod canonical-dir ((dir symbol))
  (canonical-dir (string dir)))
