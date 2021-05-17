(load (make-pathname :directory '(:relative "src") :name "vehicle" :type "cl"))
(load (make-pathname :directory '(:relative "src") :name "draw" :type "cl"))
(load (make-pathname :directory '(:relative "src") :name "run" :type "cl"))

(defun main ()
  "- For each vehicle, create a vehicle structure with:
          - The read user vehicle.
          - The vehicle's grid. The grid keeps track of which driver calls each part has called. It's also used for displaying the vehicle.
          - List of pending actions, if any
  - Main loop:
          - Run each vehicle, collecting actions, but not executing them yet.
          - Execute all the pending actions for every vehicle.
          - Draw each vehicle."

  (let (vehicles)
    (dolist (file (cdr *posix-argv*))
      (push (make-instance 'vehicle :file file) vehicles))

    (do nil ((= 1 (length vehicles)))
      (query vehicles)
      (run vehicles)
      (draw vehicles)
      (read-char)))
