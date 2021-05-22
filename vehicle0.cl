(let
  (
    (top
      (lambda ()
        (let ((sensor-results (driver:call 2 nil)))     ;; Call the sensor and get it's results
          (if sensor-results
            (driver:call 1 nil))
          nil)))
    (sensor
      (lambda ()
        (driver:sense -1 0)))
    (gun
      (lambda ()
        (driver:shoot -1 0))))
  `(,top nil (,gun nil nil nil nil) (,sensor nil nil nil nil) nil))
