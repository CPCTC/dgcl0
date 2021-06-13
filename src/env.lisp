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
     :initform (make-hash-table :test #'equal))))
  ;; more coming soon...

(defmacro do-vehicle ((v-sym worldstate) &body b)
  `(block do-vehicle
    (dolist (,v-sym (slot-value ,worldstate 'vehicles))
      ,@b)))

(defmacro do-grid ((node-sym pos-sym worldstate) &body b)
  (let ((key-sym (gensym))
        (value-sym (gensym)))
    `(maphash
       (lambda (,key-sym ,value-sym)
         (when (eql (type-of ,key-sym) 'node)
           (let ((,node-sym ,key-sym)
                 (,pos-sym  ,value-sym))
             ,@b)))
       (slot-value ,worldstate 'grid))))

(defmacro get-grid-elt (worldstate designator)
  `(gethash ,designator (slot-value ,worldstate 'grid)))

(define-condition collision () ())

(defun add-grid-elt (worldstate node pos)
  (when (gethash pos (slot-value worldstate 'grid))
    (error 'collision))
  (setf (gethash pos (slot-value worldstate 'grid)) node)
  (setf (gethash node (slot-value worldstate 'grid)) pos))

(defun rm-grid-elt (worldstate designator)
  (remhash (gethash designator (slot-value worldstate 'grid)) (slot-value worldstate 'grid))
  (remhash designator (slot-value worldstate 'grid)))

(defun add-vehicle-nodes (worldstate vehicle)
  (let (added)
    (douv (node dir (top vehicle))
      (let ((pos
              (local->global-pos vehicle (dir->coords dir))))
        (handler-case
          (add-grid-elt worldstate node pos)
          (collision (er)
            (dolist (a added)
              (rm-grid-elt worldstate a))
            (error er)))
        (push node added)))))

(defun rm-vehicle-nodes (worldstate vehicle)
  (douv (node dir (top vehicle))
    (declare (ignore dir))
    (rm-grid-elt worldstate node)))

(defun add-vehicle (worldstate v)
  (push v (slot-value worldstate 'vehicles))
  (add-vehicle-nodes worldstate v))
  ;; also move on in the list checked by next-vehicle-pos

(defmacro rm-vehicle (worldstate vehicle)
  `(let ((worldstate ,worldstate))
     (setf (slot-value worldstate 'vehicle) (delete ,vehicle (slot-value worldstate 'vehicle)))))

(defmacro done-p (worldstate)
  `(= (length (slot-value ,worldstate 'vehicles)) 1))

(defun next-vehicle-pos ()
  ;; check the worldstate (the map) for a good place to put it.

  ;; but for now....
  '(0 0))

;;; *Worldstate* var ;;;

(defvar *worldstate*
  (make-instance 'env))