(provide 'run)

(require 'driver (make-pathname :directory '(:relative "src") :name "driver" :type "cl"))
(require 'const (make-pathname :directory '(:relative "src") :name "const" :type "cl"))

;; bullets: '(pos vel count)
(defun step-bullets (bullets vehicles)
  (dolist (b bullets)
    (destroy-location (first b) vehicles)
    (setf (first b) (mapcar #'+ (first b) (second b)))
    (incf (third b))
    (if (> (third b) max-bullet-lifetime)
      (remove b bullets)
      (destroy-location (first b) vehicles)))
  (values bullets vehicles))

(defun run-vehicle (*this-vehicle* *other-vehicles*)
  (declare (special *this-vehicle* *other-vehicles*))
  (let (*driver-directions*)
    (declare (special *driver-directions*))
    (handler-case
      (funcall (first (user-vehicle *this-vehicle*)))
      (condition nil
        (remove *this-vehicle* *other-vehicles*))))
  (values *other-vehicles*))

(defun run (vehicles bullets)
  (multiple-value-setq (bullets vehicles)
    (step-bullets bullets vehicles))
  (dolist (v vehicles)
    (setf vehicles
      (run-vehicle v vehicles)))
  (values vehicles bullets))
