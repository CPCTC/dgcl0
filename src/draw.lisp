(in-package :dgcl0)

(defun find-size (worldstate)
  (let (poses)
    (do-grid (node pos worldstate)
      (declare (ignore node))
      (push pos poses))
    (let* ((xs
             (mapcar #'second poses))
           (ys
             (mapcar #'first poses))
           (min-pos
             (list (apply #'min ys) (apply #'min xs)))
           (max-pos
             (list (apply #'max ys) (apply #'max xs)))
           (size
             (mapcar #'+ (mapcar #'- max-pos min-pos) '(1 1)))
           (origin
             (mapcar #'- min-pos)))
      (values size origin))))

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
      (display field size))))
