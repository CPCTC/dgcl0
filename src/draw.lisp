(in-package :dgcl0-int)

(defun find-size (worldstate)
  (multiple-value-bind (min-pos max-pos) (world-size worldstate)
    (values
      (mapcar #'+ (mapcar #'- max-pos min-pos) '(1 1))
      (mapcar #'- min-pos))))

(defun plot (field width pos char)
  (setf
    (elt field
         (+ (* (first pos) width)
            (second pos)))
    char))

(defun display (field field-size)
  (let ((counter 0))
    (dotimes (i (apply #'* field-size))
      (when (>= counter (second field-size))
        (setf counter 0)
        (format t "~%"))
      (format t "~a " (elt field i))
      (incf counter))))

(defun draw (worldstate)
  (multiple-value-bind (size origin) (find-size worldstate)
    (let ((field
            (make-array (apply #'* size) :initial-element #\Space)))
      (do-grid (node pos worldstate)
        (plot field (second size) (mapcar #'+ pos origin) (node-char node)))
      (dolist (b (bullets worldstate))
        (plot field (second size) (mapcar #'+ (pos b) origin) (obj-type-char 'bullet)))
      (display field size))))
