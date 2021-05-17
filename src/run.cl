(provide 'run)

(defun run-vehicle (*this-vehicle* *other-vehicles*)
  (declare (special *this-vehicle* *other-vehicles*))
  (let (*driver-directions*)
    (declare (special *driver-directions*))
    (handler-case (funcall (first (slot-value *this-vehicle* 'user-vehicle)))
      (condition nil
        (setf *other-vehicles* (delete *this-vehicle* *other-vehicles*))))))

(defun step-bullet (bullet vehicles)
  ...)

(defun run (vehicles bullets)
  (dolist (v vehicles)
    (run-vehicle v vehicles))
  (dolist (b bullets)
    (step-bullet b vehicles)))
