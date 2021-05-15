(defun move-dir (coords dir)
  (let ((add (elt '((0 1) (-1 0) (0 -1) (1 0)) dir)))
    `(,(+ (first coords) (first add)) ,(+ (second coords) (second add)))))
