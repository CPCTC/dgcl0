(provide 'run)

(require 'const (make-pathname :directory '(:relative "src") :name "const" :type "cl"))

;; bullets: '(pos vel count)
(defun step-bullets (bullets vehicles)
  (dolist (b bullets)
    (destroy-location (first b) vehicles)
    (setf (first b) (mapcar #'+ (first b) (second b)))
    (incf (third b))
    (if (> (third b) max-bullet-lifetime)
      (remove b bullets)
      (destroy-location (first b) vehicles))))

(defun run-vehicle (*this-vehicle* *other-vehicles*)
  (declare (special *this-vehicle* *other-vehicles*))
  (let (*driver-directions*)
    (declare (special *driver-directions*))
    (handler-case (funcall (first (slot-value *this-vehicle* 'user-vehicle)))
      (condition nil
        (remove *this-vehicle* *other-vehicles*)))))

(defun run (vehicles bullets)
  (step-bullets bullets vehicles)
  (dolist (v vehicles)
    (run-vehicle v vehicles)))
