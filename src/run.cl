;;; Run every vehicle, collecting
;;; a list of actions in each.
(defun collect (vehicles)
  ; ...)

;;; Execute all actions from all vehicles.
(defun run (vehicles)
  (let (actions)
    (dolist (v vehicles)
      (dolist (a (slot-value v 'actions))
        (push a actions)))
    (dolist (a actions)
      (funcall a vehicles))))
