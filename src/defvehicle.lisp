;;;; Defines defvehicle, the DSL for vehicle creation.

(in-package :dgcl0-int)

;; Evaluate *just* the lambda of every node spec.
(defmacro prepare-node-specs (inc-node-specs)
  (let ((form '(list)))
    (dolist (ins inc-node-specs (reverse form))
      (push
        `(cons ,(car ins) ',(cdr ins))
        form))))

;; Create a node out of every node-spec.
;; Return a hash table:
;;   pos -> (list node no-connect-sides)
;; Also return the top node.
(defun parse-nodes (node-specs)
  (let (top
        (node-table
          (make-hash-table :test #'equal)))
    (dolist (node-spec node-specs)
      (apply
        (lambda (lmbda pos &rest no-connect-sides)
          (let ((node (make-node lmbda)))
            (setf
              (gethash pos node-table)
              (list node (mapcar #'canonical-dir no-connect-sides)))
            (unless top
              (setf top node))))
        node-spec))
    (values node-table top)))

;; Connect adjacent nodes in the
;; node-table (the format of which
;; is like the one returned by
;; #'parse-nodes).
(defun connect-nodes (node-table)
  (maphash
    (lambda (pos val)
      (apply
        (lambda (node no-connect-sides)
          (dotimes (i 4)
            (apply
              (lambda (&optional other-node other-no-connect-sides)
                (unless (or
                          (not other-node)
                          (find i no-connect-sides)
                          (find (opposite-dir i) other-no-connect-sides))
                  (setf (connection node i) other-node)
                  (setf (connection other-node (opposite-dir i)) node)))
              (gethash (move-dir pos i) node-table))))
        val))
    node-table))

(defmacro defvehicle (name &key (nodes (error "This vehicle has no nodes.")))
  `(add-vehicle
     *default-worldstate*
     (make-vehicle
       ,name
       (multiple-value-bind (node-table top) (parse-nodes (prepare-node-specs ,nodes))
         (connect-nodes node-table)
         top)
       (next-vehicle-pos))))
