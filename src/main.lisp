;;;; This main function is unnecessary. If you want,
;;;; you can instead load a bunch of vehicles with
;;;; defvehicle, then start the simulation with
;;;; (run *worldstate*). This function is mostly
;;;; for creating standalone binaries.

(in-package :dgcl0)

(defun main (argv)
  ;; Make sure we have everything needed to compile a vehicle
  (require :dgcl0-defvehicle :dgcl0-driver)

  (dolist (file (cdr argv))     ; assuming posix-style argv
    (load file))
  (run *worldstate*))
