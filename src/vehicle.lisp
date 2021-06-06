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
     :initform (make-array 4 :initial-element nil))
   (char
     :accessor :node-char
     :initform #\.)))

(defmacro make-node (lambda)
  `(make-instance 'node :lambda ,lambda))

(defmacro connection (node n)
  `(elt (slot-value ,node 'connections) ,n))

;;; Utility functions ;;;

;; All of these use a coordinate system
;; that ignores vehicle location and is
;; relative to the top node.

(defun move-dir (coords dir)
  (mapcar #'+ coords
    (elt '((0 1) (-1 0) (0 -1) (1 0)) dir)))

(defun dir->coords (directions)
  (let ((coords '(0 0)))
    (dolist (d directions)
      (setf coords (move-dir coords d)))
    coords))

(defun douv-impl (fn node &optional (visited (make-hash-table :test #'equal)) reverse-dir)
  (let* ((coords
           (dir->coords reverse-dir))
         (already-here-p
           (second (multiple-value-list (gethash coords visited)))))
    (when (and node (not already-here-p))
      (funcall fn node (reverse reverse-dir))
      (setf (gethash coords visited) t)
      (dotimes (i 4)
        (douv-impl fn (connection node i) visited (cons i reverse-dir))))))

(defmacro douv ((node-sym dir-sym top) &body b)
  `(block douv
    (douv-impl
      (lambda (,node-sym ,dir-sym)
        ,@b)
      ,top)))
