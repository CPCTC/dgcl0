(provide 'vehicle)

(require 'const (make-pathname :directory '(:relative "src") :name "const" :type "cl"))

;;; Vehicles have the following stuff in em.
;;;         - user vehicle: single form read from file.
;;;         - grid: hash table of coordinates -> directions and a character

(defclass vehicle nil
  (user-vehicle
   (pos
     :initform '(0 0))
   grid))

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

(defun node (user-vehicle directions)
  (if directions
    (node (elt user-vehicle (1+ (car directions))) (cdr directions))
    user-vehicle))
