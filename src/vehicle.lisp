#|
;;; Vehicles have the following stuff in em.
;;;         - user vehicle: single form read from file.
;;;         - pos: position of top node on global grid.

(defmacro new-vehicle (user-vehicle pos)
  `(list ,user-vehicle ,pos))

(defmacro user-vehicle (vehicle)
  `(first ,vehicle))

(defmacro pos (vehicle)
  `(second ,vehicle))

(defmacro child (user-vehicle child)
  `(elt ,user-vehicle (+ 2 ,child)))

(defmacro uv-char (user-vehicle)
  `(first ,user-vehicle))

(defmacro uv-func (user-vehicle)
  `(second ,user-vehicle))

(defun push-chars (user-vehicle)
  (when user-vehicle
    (push (gethash nil drcall->char) user-vehicle)
    (dotimes (i 4)
      (setf (child user-vehicle i)
        (push-chars (child user-vehicle i))))
    user-vehicle))

(defun make-vehicle-file (file)
  (let (
        (user-vehicle
          (with-open-file (in file)
            (eval (read in)))))
    (new-vehicle (push-chars user-vehicle) '(0 0))))

(defun node (user-vehicle directions)
  (if directions
    (node (child user-vehicle (car directions)) (cdr directions))
    user-vehicle))
|#

;;;; Defines the vehicle class, it's subclasses, thier accessors,
;;;; and some convienence functions. Only stuff that relates to
;;;; the vehicle data structure should go here, logic should go
;;;; in vehicle-logic.lisp.

(in-package :dgcl0)

(defclass vehicle nil
  ((pos
     :accessor :pos
     :initarg :pos)
   (top
     :accessor :top
     :initarg :top)
   (rotation
     :accessor :rotation
     :initform 0)))

(defmacro make-vehicle (top &optional (pos (list 0 0)))
  `(make-instance 'vehicle :top ,top :pos ,pos))

(defclass node nil
  ((lambda
     :accessor :node-lambda
     :initarg :lambda)
   (connections
     :initarg :connections)
   (char
     :accessor :node-char
     :initform #\.)))

(defmacro make-node (lambda &optional (connections (make-array 4 :initial-element nil)))
  `(make-instance 'node :lambda ,lambda :connections ,connections))

(defun connection (node n)
  (declare (type node node))
  (when (and (>= n 0) (< n 4))
    (elt (slot-value node 'connections) n)))

(defun move-dir (coords dir)
  (mapcar #'+ coords
    (elt '((0 1) (-1 0) (0 -1) (1 0)) dir)))

(defun dir->coords (directions)
  (let ((coords '(0 0)))
    (dolist (d directions)
      (setf coords (move-dir coords d)))
    coords))
