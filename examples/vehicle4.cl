(let
  (
    (top
      (lambda ()
        (let ((sensor-results (funcall (dgcl0-driver:neighbor :west))))
          (if (not (char= sensor-results #\Space))
            (funcall (dgcl0-driver:neighbor :north))))))
    (sensor
      (lambda ()
        (dgcl0-driver:sense 0 -1)))
    (gun
      (lambda ()
        (dgcl0-driver:shoot 0 -4)
        (funcall (dgcl0-driver:neighbor :left)))))
  (dgcl0:defvehicle "Vehicle5"
    :nodes
    ((top (0 0))
     (sensor (0 -1))
     (gun (-1 0))
     (gun (-1 -1))
     (gun (-1 -2))
     (gun (-1 -3)))))
