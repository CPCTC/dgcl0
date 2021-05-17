;;; Run every vehicle, collecting
;;; a list of actions in each.
(defun collect (vehicles)
  ; ...)

;;; Execute all actions from all vehicles.
(defun run (vehicles)
  (let (actions)
    (dolist (v vehicles)
      (dolist (a (slot-value v 'actions))
        (push '(,a ,v) actions)))
    (dolist (a actions)
      (apply (first a) `(,(second a) ,vehicles))))))
