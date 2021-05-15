(load (make-pathname :directory '(:relative "src") :name "const" :type "cl"))

;;; Vehicles have the following stuff in em.
;;;         - user vehicle: single form read from file.
;;;         - grid: hash table of coordinates -> directions and a character
;;;         - actions: list of pending actions

(defclass vehicle nil
  (user-vehicle
   grid             ; name is misleading, should be called `data`
   (actions
     :initform nil)))

(defun make-grid (user-vehicle &optional (reverse-directions nil) (grid (make-hash-table)))
  (cond
    (user-vehicle
      (let ((coords '(0 0)))
        (dolist (dir reverse-directions)
          (setf coords (move-dir coords dir)))
        (setf (gethash coords grid) `(,(reverse reverse-directions) ,(gethash nil drcall->char))))
      (dotimes (i 4)
        (make-grid (elt user-vehicle (1+ i)) (cons i reverse-directions) grid))
      grid)
    (T
      nil)))

(defmethod initialize-instance :after ((v vehicle) &key file)
  (if (not file)
    (error "Must specify :file."))
  (let (
        (user-vehicle
          (with-open-file (in file)
            (eval (read in)))))
    (setf (slot-value v 'user-vehicle) user-vehicle)
    (setf (slot-value v 'grid)
      (make-grid user-vehicle))))
