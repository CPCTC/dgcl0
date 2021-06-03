#|
;; bullets: '(pos vel count)
(defun step-bullets (bullets vehicles)
  (dolist (b bullets)
    (setf vehicles (destroy-location (first b) vehicles))
    (setf (first b) (mapcar #'+ (first b) (second b)))
    (incf (third b))
    (if (> (third b) max-bullet-lifetime)
      (remove b bullets)
      (setf vehicles (destroy-location (first b) vehicles))))
  (values bullets vehicles))

(defun step-vehicle (*this-vehicle* *other-vehicles*)
  (declare (special *this-vehicle* *other-vehicles*))
  (let (*driver-directions*)
    (declare (special *driver-directions*))
    (handler-case
      (funcall (uv-func (user-vehicle *this-vehicle*)))
      (condition nil
        (remove *this-vehicle* *other-vehicles*))))
  (values *other-vehicles*))

(defun timestep (vehicles bullets)
  (multiple-value-setq (bullets vehicles)
    (step-bullets bullets vehicles))
  (dolist (v vehicles)
    (setf vehicles
      (step-vehicle v vehicles)))
  (values vehicles bullets))

|#
