;;; Contains everything having to do
;;; with directions

(in-package :dgcl0-int)

;;; Canonical-dir generic ;;;

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

;;; Other direction functions ;;;

(defmacro opposite-dir (dir)
  `(mod (+ ,dir 2) 4))

(defun move-dir (coords dir)
  (mapcar #'+ coords
    (elt '((0 1) (-1 0) (0 -1) (1 0)) dir)))

(defun dir->coords (directions)
  (let ((coords '(0 0)))
    (dolist (d directions)
      (setf coords (move-dir coords d)))
    coords))

;; rotate *pos* *rotation* times
;; counterclockwise around (0 0)
(defun rotate (pos rotation)
  (dotimes (i rotation pos)
    (setf pos (list (- (second pos)) (first pos)))))

(defun local->global-pos (vehicle pos)
  (mapcar #'+ (pos vehicle) (rotate pos (rotation vehicle))))
