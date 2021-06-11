;;;; Provides the env class, and is responsible
;;;; for low-level manipulations of *worldstate*.

(in-package :dgcl0)

;;; Env class ;;;

(defclass env nil
  ((vehicles
     :initform nil)
   (grid
     :accessor grid
     :initform (make-hash-table :test #'equal))))
  ;; more coming soon...

(defmacro do-vehicle ((v-sym worldstate) &body b)
  `(block do-vehicle
    (dolist (,v-sym (slot-value ,worldstate 'vehicles))
      ,@b)))

(defmacro add-vehicle (worldstate v)
  `(push ,v (slot-value ,worldstate 'vehicles)))
  ;; also move on in the list checked by next-vehicle-pos

(defmacro rm-vehicle (worldstate vehicle)
  `(let ((worldstate ,worldstate))
     (setf (slot-value worldstate 'vehicle) (delete ,vehicle (slot-value worldstate 'vehicle)))))

(defmacro done-p (worldstate)
  `(= (length (slot-value ,worldstate 'vehicles)) 1))

;; Set up the grid. TODO
(defun initialize (worldstate)
  (declare (ignore worldstate)))

(defun next-vehicle-pos ()
  ;; check the worldstate (the map) for a good place to put it.

  ;; but for now....
  (list (- (random 200) 100) (- (random 200) 100)))

;;; *Worldstate* var ;;;

(defvar *worldstate*
  (make-instance 'env))
