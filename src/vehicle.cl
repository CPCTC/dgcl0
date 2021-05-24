(provide 'vehicle)

(require 'const (make-pathname :directory '(:relative "src") :name "const" :type "cl"))

;;; Vehicles have the following stuff in em.
;;;         - user vehicle: single form read from file.
;;;         - pos: position of top node on global grid.

(defmacro new-vehicle (user-vehicle pos)
  `(list ,user-vehicle ,pos))

(defmacro user-vehicle (vehicle)
  `(first ,vehicle))

(defmacro pos (vehicle)
  `(second ,vehicle))

(defmacro child (user-vehicle child)
  `(elt ,user-vehicle (+ 2 ,child)))

(defun push-chars (user-vehicle)
  (when user-vehicle
    (push (gethash nil drcall->char) user-vehicle)
    (dotimes (i 4)
      (setf (child user-vehicle i)
        (push-chars (child user-vehicle i))))
    user-vehicle))

(defun make-vehicle-file (file)
  (let (
        (user-vehicle
          (with-open-file (in file)
            (eval (read in)))))
    (new-vehicle (push-chars user-vehicle) '(0 0))))

(defun node (user-vehicle directions)
  (if directions
    (node (child user-vehicle (car directions)) (cdr directions))
    user-vehicle))
