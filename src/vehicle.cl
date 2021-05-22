(provide 'vehicle)

(require 'const (make-pathname :directory '(:relative "src") :name "const" :type "cl"))

;;; Vehicles have the following stuff in em.
;;;         - user vehicle: single form read from file.
;;;         - grid: hash table of coordinates -> directions and a character

(defun user-vehicle (vehicle)
  (first vehicle))

(defun pos (vehicle)
  (second vehicle))

(defun grid (vehicle)
  (third vehicle))

(defun make-grid (user-vehicle &optional (reverse-directions nil) (grid (make-hash-table :test #'equal)))
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

(defun make-vehicle-file (file)
  (let (
        (user-vehicle
          (with-open-file (in file)
            (eval (read in)))))
    `(,user-vehicle (0 0) ,(make-grid user-vehicle))))

(defun node (user-vehicle directions)
  (if directions
    (node (elt user-vehicle (1+ (car directions))) (cdr directions))
    user-vehicle))
