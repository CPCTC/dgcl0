;;;; Contains implementations of all drcalls
;;;; and other complex vehicle logic.

(in-package :dgcl0-int)

(defun dgcl0-driver:neighbor (dir)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (let ((node (get-grid-elt
                *worldstate*
                (move-dir
                  (get-grid-elt *worldstate* *this-node*)
                  (canonical-dir dir)))))
    (if node
      (values
        (lambda (&rest args)
          (let ((*this-node* node))
            (declare (special *this-node*))
            (apply (node-lambda node) args)))
        t)
      (values
        (lambda (&rest r)
          (declare (ignore r)))
        nil))))
