#|
(defun release-child (vehicle directions child vehicles)
  (let* (
         (child-directions
           (append directions (list child)))
         (child-uv
           (node
             (user-vehicle vehicle)
             child-directions))
         (child-pos       ;; on global grid
           (mapcar #'+
             (pos vehicle)
             (dir->coords child-directions))))
    (when child-uv
      (setf
        (child
          (node
            (user-vehicle vehicle)
            directions)
          child)
        nil)
      (push (new-vehicle child-uv child-pos) vehicles))
    vehicles))

(defun destroy-node (vehicle directions vehicles)
  (dotimes (i 4)
    (setf vehicles (release-child vehicle directions i vehicles)))
  (if directions
    (setf
      (child
        (node
          (user-vehicle vehicle)
          (reverse (cdr (reverse directions))))
        (car (last directions)))
      nil)
    (setf vehicles (delete vehicle vehicles)))
  vehicles)

(defun douv-impl (fn uv &optional reverse-dir)
  (when uv
    (funcall fn uv (reverse reverse-dir))
    (dotimes (i 4)
      (douv-impl fn (child uv i) (cons i reverse-dir)))))

(defmacro douv ((uv-sym dir-sym uv) &body b)
  `(block douv
    (douv-impl
      (lambda (,uv-sym ,dir-sym)
        ,@b)
      ,uv)))

(defun all-nodes (vehicles)
  (let ((nodes (make-hash-table :test #'equal)))
    (dolist (v vehicles nodes)
      (douv (uv dir (user-vehicle v))
        (declare (ignore uv))
        (setf (gethash (mapcar #'+ (dir->coords dir) (pos v)) nodes) (list v dir))))))

(defgeneric query-location (data coords))

(defmethod query-location ((data hash-table) coords)
  (values-list (gethash coords data (list nil nil))))

(defmethod query-location ((data list) coords)  ;; if list of vehicles
  (query-location (all-nodes data) coords))

(defun destroy-location (coords vehicles)
  (multiple-value-bind (vehicle directions) (query-location vehicles coords)
    (if vehicle
      (setf vehicles (destroy-node vehicle directions vehicles)))
    vehicles))
|#
