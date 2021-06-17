;;;; Highest-level function for the simulation proper.

(in-package :dgcl0-int)

(defun step-vehicle (*worldstate* *this-vehicle*)
  (declare (special *worldstate* *this-vehicle*))
  (let ((*this-node* (top *this-vehicle*)))
    (declare (special *this-node*))
    (handler-case
      (funcall (node-lambda *this-node*))
      (condition ()
        (rm-vehicle *worldstate* *this-vehicle*)))))

(defun timestep (worldstate)
  (step-env worldstate)
  (do-vehicle (v worldstate)
    (step-vehicle worldstate v)))

(defun run (&optional (worldstate *default-worldstate*))
  (do () ((done-p worldstate))
    (timestep worldstate)
    (draw worldstate)
    (read-char)))
