(let ((top
        (lambda ()
          (dgcl0-driver:shoot
            (- (random 5) 2)
            (- (random 5) 2)))))
  (dgcl0:defvehicle "Bullet Hell" :nodes
    ((top (0 0)))))
