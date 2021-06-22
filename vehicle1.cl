(let
  (
    (top
      (lambda ()
        (let ((sensor-results (funcall (dgcl0-driver:neighbor :west))))
          (if (not (char= sensor-results #\Space))
            (funcall (dgcl0-driver:neighbor :north))))))
    (sensor
      (lambda ()
        (dgcl0-driver:sense -1 0)))
    (gun
      (lambda ()
        (dgcl0-driver:shoot -1 0))))
  (dgcl0:defvehicle "Vehicle1"
    :nodes
    ((top (0 0))
     (sensor (0 -1))
     (gun (-1 0)))))
