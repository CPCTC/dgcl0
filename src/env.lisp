;;;; Provides the env class, and is responsible
;;;; for low-level manipulations of *worldstate*.

(in-package :dgcl0)

;;; Env class ;;;

(defclass env nil
  ((vehicles
     :initform nil)
   ;; Grid holds both
   ;; global-pos -> node
   ;; node -> global-pos
   (grid
     :accessor grid
     :initform (make-hash-table :test #'equal))))
  ;; more coming soon...

(defmacro do-vehicle ((v-sym worldstate) &body b)
  `(block do-vehicle
    (dolist (,v-sym (slot-value ,worldstate 'vehicles))
      ,@b)))

(define-condition collision () ())

(defun add-grid-nodes (worldstate vehicle)
  (let (added)
    (douv (node dir (top vehicle))
      (let ((pos
              (local->global-pos vehicle (dir->coords dir))))
        (when (gethash pos (grid worldstate))
          (dolist (a added)
            (remhash a (grid worldstate)))
          (error 'collision))
        (setf (gethash pos (grid worldstate)) node)
        (push pos added)
        (setf (gethash node (grid worldstate)) pos)
        (push node added)))))

(defun add-vehicle (worldstate v)
  (push v (slot-value worldstate 'vehicles))
  (add-grid-nodes worldstate v))
  ;; also move on in the list checked by next-vehicle-pos

(defmacro rm-vehicle (worldstate vehicle)
  `(let ((worldstate ,worldstate))
     (setf (slot-value worldstate 'vehicle) (delete ,vehicle (slot-value worldstate 'vehicle)))))

(defmacro done-p (worldstate)
  `(= (length (slot-value ,worldstate 'vehicles)) 1))

(defun next-vehicle-pos ()
  ;; check the worldstate (the map) for a good place to put it.

  ;; but for now....
  (list (- (random 200) 100) (- (random 200) 100)))

;;; *Worldstate* var ;;;

(defvar *worldstate*
  (make-instance 'env))
