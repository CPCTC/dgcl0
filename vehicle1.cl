(let
  (
    (top
      (lambda ()
        (let ((sensor-results (funcall (driver:neighbor :west))))
          (if sensor-results
            (funcall (driver:neighbor :north))))))
    (sensor
      (lambda ()
        (driver:sense -1 0)))
    (gun
      (lambda ()
        (driver:shoot -1 0))))
  (dgcl0:defvehicle "Vehicle1"
    :nodes
    ((top (0 0))
     (sensor (0 -1))
     (gun (-1 0)))))
