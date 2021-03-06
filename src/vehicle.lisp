;;;; Defines the vehicle class, it's subclasses, thier accessors,
;;;; and some convienence functions. Only stuff that relates to
;;;; the vehicle data structure should go here, logic should go
;;;; in vehicle-logic.lisp.

(in-package :dgcl0-int)

;;; Vehicle class ;;;

(defclass vehicle nil
  ((name
     :accessor name
     :initarg :name)
   (pos
     :accessor pos
     :initarg :pos)
   (top
     :accessor top
     :initarg :top)
   (rotation
     :accessor rotation
     :initform 0)))

(defmacro make-vehicle (name top pos)
  `(make-instance 'vehicle :name ,name :top ,top :pos ,pos))

;;; Node class ;;;

(defclass node nil
  ((lambda
     :accessor node-lambda
     :initarg :lambda)
   (connections
     :initform (make-array 4 :initial-element nil))
   (char
     :accessor node-char
     :initform (obj-type-char 'unknown))
   (type
     :accessor node-type
     :initform 'unknown)))

(defmacro make-node (lambda)
  `(make-instance 'node :lambda ,lambda))

(defmacro connection (node n)
  `(elt (slot-value ,node 'connections) ,n))

(defun assert-node-type (node sym)
  (if (eql (node-type node) 'unknown)
    (progn
      (setf (node-type node) sym)
      (setf (node-char node) (obj-type-char sym)))
    (unless (eql (node-type node) sym)
      (error "Node of type ~a tried to ~a." (node-type node) sym))))

;;; Utility functions ;;;

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

(defun connected-p (a b)
  (douv (node dir a)
    (declare (ignore dir))
    (when (eq node b)
      (return-from connected-p t)))
  nil)
