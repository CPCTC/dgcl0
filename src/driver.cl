(provide 'driver)

(require 'const (make-pathname :directory '(:relative "src") :name "const" :type "cl"))
(require 'vehicle (make-pathname :directory '(:relative "src") :name "vehicle" :type "cl"))

(defpackage driver
  (:documentation "Public interface for calls to the dgcl0 driver from loaded vehicles.")
  (:export
    call	;; Call other functions, but the driver can track your location.
    sense	;; Gather information from the environment.
    shoot	;; Fire a projectile that can destroy nodes.
    rotate      ;; Reorder your four children.
    re-top      ;; Set the calling-node as the new top level for your entire vehicle.
    re-parent   ;; Pick a new parent for this node.
    translate   ;; Shift your vehicle in one of the four cardinal directions.
    children    ;; Check which children are still attached.
    release     ;; Split off a child.
    explode     ;; Destroy the area around the caller.
    ))

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
    (release-child vehicle directions i vehicles))
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
