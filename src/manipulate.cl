(provide 'manipulate)

(require 'const (make-pathname :directory '(:relative "src") :name "const" :type "cl"))
(require 'vehicle (make-pathname :directory '(:relative "src") :name "vehicle" :type "cl"))

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
             (dir->coords child-directions)))
         (child-grid
           (make-grid child-uv)))
    (setf
      (child
        (node
          (user-vehicle vehicle)
          directions)
        child)
      nil)
    (maphash
      (lambda (k v)
        (let ((child-grid-key
                (mapcar #'- k (dir->coords child-directions))))
          (when (gethash child-grid-key child-grid)
            (remhash k (grid vehicle))
            (setf (second (gethash child-grid-key child-grid)) (second v)))))
      (grid vehicle))
    (push (new-vehicle child-uv child-pos child-grid) vehicles)))

(defun destroy-node (vehicle directions vehicles)
  (dotimes (i 4)
    ;; As children are released, thier grid entries
    ;; in the parent vehicle are removed and transferred
    ;; to the new vehicle.
    (setf vehicles (release-child vehicle directions i vehicles)))
  (remhash (dir->coords directions) (grid vehicle))
  (setf
    (child
      (node
        (user-vehicle vehicle)
        (reverse (cdr (reverse directions))))
      (car (last directions)))
    nil)
  vehicles)

(defun destroy-location (coords vehicles)
  (dolist (v vehicles)
    (maphash
      (lambda (key value)
        (when (equal key coords)
          (setf vehicles (destroy-node v (first value) vehicles))
          (return-from destroy-location vehicles)))
      (grid v)))
  vehicles)
