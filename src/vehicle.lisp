;;;; Defines the vehicle class, it's subclasses, thier accessors,
;;;; and some convienence functions. Only stuff that relates to
;;;; the vehicle data structure should go here, logic should go
;;;; in vehicle-logic.lisp.

(in-package :dgcl0)

;;; Vehicle class ;;;

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

;;; Node class ;;;

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

;;; Utility functions ;;;

(defun move-dir (coords dir)
  (mapcar #'+ coords
    (elt '((0 1) (-1 0) (0 -1) (1 0)) dir)))

(defun dir->coords (directions)
  (let ((coords '(0 0)))
    (dolist (d directions)
      (setf coords (move-dir coords d)))
    coords))
