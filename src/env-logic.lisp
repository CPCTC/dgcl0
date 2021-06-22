;;;; Contains the logic of all non-vehicle environment objects.

(in-package :dgcl0-int)

(defconstant bullet-lifetime 10)

(defun step-bullet (worldstate bullet)
  (when (>= (counter bullet) bullet-lifetime)
    (setf (bullets worldstate)
      (delete bullet (bullets worldstate)))
    (return-from step-bullet))
  (destroy-location worldstate (pos bullet))
  (setf (pos bullet)
    (mapcar #'+ (pos bullet) (vel bullet)))
  (incf (counter bullet)))

(defun step-env (worldstate)
  (dolist (b (bullets worldstate))
    (step-bullet worldstate b)))
