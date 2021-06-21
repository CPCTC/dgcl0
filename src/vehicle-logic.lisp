;;;; Contains implementations of all drcalls
;;;; and other complex vehicle logic.

(in-package :dgcl0-int)

(defun dgcl0-driver:neighbor (dir)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (let ((node (connection *this-node* (canonical-dir dir))))
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
  (let ((pos
          (get-grid-elt *worldstate* *this-node*))
        (vec
          (rotate (list y x) (rotation *this-vehicle*))))
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
          (rotate (list y x) (rotation *this-vehicle*))))
    (add-bullet *worldstate* (mapcar #'+ pos vel) vel)))

(defun dgcl0-driver:translate (dir)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (assert-node-type *this-node* 'translate)
  (rm-vehicle-nodes *worldstate* *this-vehicle*)
  (setf
    (pos *this-vehicle*)
    (move-dir
      (pos *this-vehicle*)
      (rotate-dir
        (rotation *this-vehicle*)
        (canonical-dir dir))))
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
  (let* ((connection-dir
          (rotate-dir (rotation *this-vehicle*) (canonical-dir dir)))
         (node
           (get-grid-elt
             *worldstate*
             (move-dir
               (get-grid-elt *worldstate* *this-node*)
               connection-dir))))
    (unless node
      (error "No node to connect to."))
    (unless (connected-p node *this-node*)
      (rm-vehicle-top *worldstate* *this-vehicle*))
    (setf (connection *this-node* (canonical-dir dir)) node)  ;; local direction
    (setf (connection node (opposite-dir (canonical-dir dir))) *this-node*)))

(defun dgcl0-driver:disconnect (dir)
  (declare (special *worldstate* *this-vehicle* *this-node*))
  (assert-node-type *this-node* 'connect-disconnect)
  (let ((node
          (connection *this-node* (canonical-dir dir))))
    (unless node
      (error "No node to disconnect from."))
    (setf (connection *this-node* (canonical-dir dir)) nil)
    (setf (connection node (opposite-dir (canonical-dir dir))) nil)
    (unless (connected-p node *this-node*)
      (let ((new-top-node
              (if (connected-p *this-node* (top *this-vehicle*))
                node
                *this-node*)))
        (add-vehicle-top *worldstate*
          (make-vehicle
            (concatenate 'string (name *this-vehicle*) " Part")
            new-top-node
            (get-grid-elt *worldstate* new-top-node)))))))
