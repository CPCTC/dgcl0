#|
* . *
|#

(let ((core
        (let ((n 4) (c 0))
          (lambda ()
            (if (< c n)
              (progn
                (funcall (dgcl0-driver:neighbor :right))
                (funcall (dgcl0-driver:neighbor :left))
                (setf c (1+ c)))
              (progn
                (dgcl0-driver:rotate :left)
                (setf c 0))))))
      (pivot
        (lambda ()
          (dgcl0-driver:rotate :down))))
  (dgcl0:defvehicle "Square Seeker" :nodes
    ((core (0 0))
     (pivot (0 1))
     (pivot (0 -1)))))
