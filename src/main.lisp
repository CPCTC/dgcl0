;;;; This main function is unnecessary. If you want,
;;;; you can instead load a bunch of vehicles with
;;;; defvehicle, then start the simulation with
;;;; (run *default-worldstate*). This function is mostly
;;;; for creating standalone binaries.

(in-package :dgcl0-int)

(defun main (argv)
  (dolist (file (cdr argv))     ; assuming posix-style argv
    (load file))
  (run *default-worldstate*))
