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

(defun dgcl0-driver:sense (y x)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (assert-node-type *this-node* 'sense)
  (let ((pos (get-grid-elt *worldstate* *this-node*))
        (vec (list y x)))
    (loop
      (progn
        ;; move to new location
        (setf pos (mapcar #'+ pos vec))
        (multiple-value-bind (min-coord max-coord) (world-size *worldstate*)
          (when
            (not    ;; when i'm outside of the grid area
              (and
                (apply #'<= (mapcar #'first (list min-coord pos max-coord)))
                (apply #'<= (mapcar #'second (list min-coord pos max-coord)))))
            (return #\Space)))
        ;; check new location
        (let ((result
                (get-grid-elt *worldstate* pos)))
          (when result
            (return (node-char result))))
        ))))

(defun dgcl0-driver:shoot (y x)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (assert-node-type *this-node* 'shoot)
  (let ((pos
          (get-grid-elt *worldstate* *this-node*))
        (vel
          (list y x)))
    (add-bullet *worldstate* (mapcar #'+ pos vel) vel)))

(defun dgcl0-driver:translate (dir)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (assert-node-type *this-node* 'translate)
  (rm-vehicle-nodes *worldstate* *this-vehicle*)
  (setf (pos *this-vehicle*) (move-dir (pos *this-vehicle*) (canonical-dir dir)))
  (add-vehicle-nodes *worldstate* *this-vehicle*))

(defun dgcl0-driver:rotate (dir)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (assert-node-type *this-node* 'rotate)
  (let* ((rotation
          (mod (1- (canonical-dir dir)) 4))
        (pivot-pos
          (get-grid-elt *worldstate* *this-node*))
        (neg-offset
          (mapcar #'- (pos *this-vehicle*) pivot-pos)))
    (rm-vehicle-nodes *worldstate* *this-vehicle*)
    (setf (rotation *this-vehicle*)
      (mod (+ (rotation *this-vehicle*) rotation) 4))
    (setf (pos *this-vehicle*)
      (mapcar #'+
        pivot-pos
        (rotate neg-offset rotation)))
    (add-vehicle-nodes *worldstate* *this-vehicle*)))

(defun dgcl0-driver:re-top ()
  (declare (special *worldstate* *this-vehicle* *this-node*))
  ;; it doesn't move, so don't mess with grid
  (setf (top *this-vehicle*) *this-node*)
  (setf (pos *this-vehicle*) (get-grid-elt *worldstate* *this-node*)))

(defun dgcl0-driver:connect (dir)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (assert-node-type *this-node* 'connect-disconnect)
  (let ((node (get-grid-elt
                *worldstate*
                (move-dir
                  (get-grid-elt *worldstate* *this-node*)
                  (canonical-dir dir)))))
    (unless node
      (error "No node to connect to."))
    (let (connected-p)
      (douv (node dir node)
        (declare (ignore dir))
        (when (eq node *this-node*)
          (setf connected-p t)))
      (unless connected-p
        (rm-vehicle-top *worldstate* *this-vehicle*)))
    (setf (connection *this-node* (canonical-dir dir)) node)
    (setf (connection node (opposite-dir (canonical-dir dir))) *this-node*)))
