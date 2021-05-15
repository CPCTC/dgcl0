;;; Vehicles have the following stuff in em.
;;;         - user vehicle: single form read from file.
;;;         - grid: hash table of coordinates -> directions and a character
;;;         - actions: list of pending actions

(defclass vehicle nil
  (user-vehicle
   grid             ; name is misleading, should be called `data`
   (actions
     :initform nil)))

(defmethod initialize-instance ((v vehicle) &key file)
  (let (
        (user-vehicle
          (with-open-file (in file)
            (read in))))
    (setf (slot-value v 'user-vehicle) user-vehicle)
    (setf (slot-value v 'grid)
      (make-grid user-vehicle))))

(defun make-grid (user-vehicle &optional (reverse-directions nil) (grid (make-hash-table)))
  (cond
    (user-vehicle
      (let ((coords '(0 0)))
        (dolist (dir reverse-directions)
          (setf coords (move-dir coords dir)))
        (setf (gethash coords grid) `(,(reverse reverse-directions) ,(drcall-char nil))))
      (dotimes (i 4)
        (make-grid (elt user-vehicle (1+ i)) (push i reverse-directions) grid))
      grid)
    (T
      nil)))
