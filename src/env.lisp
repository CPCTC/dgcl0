;;;; Provides the env class, and is responsible
;;;; for low-level manipulations of *worldstate*.

(in-package :dgcl0)

(defclass env nil
  ((vehicles
     :initform nil)
   (grid
     :accessor grid
     :initform (make-hash-table :test #'equal))))
  ;; more coming soon...

(defvar *worldstate*
  (make-instance 'env))

(defmacro add-vehicle (worldstate v)
  `(push ,v (slot-value worldstate 'vehicles)))
  ;; also move on in the list checked by next-vehicle-pos

(defun next-vehicle-pos ()
  ;; check the worldstate (the map) for a good place to put it.

  ;; but for now....
  (list (- (random 200) 100) (- (random 200) 100)))
