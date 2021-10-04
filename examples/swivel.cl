(let ((core
        (let ((dir 3)
              (count 0))
          (lambda ()
            (if (<= count 9)
              (progn
                (setf dir (mod (+ dir 2) 4))
                (incf count))
              (setf count 0))
            (funcall (dgcl0-driver:neighbor dir)))))
      (swivel
        (lambda ()
          ;; do a 180
          (dgcl0-driver:rotate 3))))
  (dgcl0:defvehicle "Swivelator"
    :nodes
    ((core (0 0))
     (swivel (1 0))
     (swivel (-1 0)))))
