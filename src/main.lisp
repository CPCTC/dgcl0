(defun main ()
  "- For each vehicle, create a vehicle structure with:
          - The read user vehicle.
          - The vehicle's grid. The grid keeps track of which driver calls each part has called. It's also used for displaying the vehicle.
  - Main loop:
          - Run each vehicle, executing actions. Step the world state.
          - Draw each vehicle."

  (let (vehicles)
    (dolist (file (cdr *posix-argv*))
      (push (make-vehicle-file file) vehicles))

    (let (bullets)
      (do nil ((= 1 (length vehicles)))
        (multiple-value-setq (vehicles bullets) (timestep vehicles bullets))
        (draw vehicles bullets)
        (read-char)))))
