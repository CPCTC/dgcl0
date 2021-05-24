(provide 'draw)

(require 'const (make-pathname :directory '(:relative "src") :name "const" :type "cl"))
(require 'vehicle (make-pathname :directory '(:relative "src") :name "vehicle" :type "cl"))

(defun uv-points (uv &optional reverse-directions)
  (if uv
    (let (points)
      (push
        (list (dir->coords (reverse reverse-directions)) (uv-char uv))
        points)
      (dotimes (i 4)
        (let ((child-points
                (uv-points (child uv i) (cons i reverse-directions))))
          (if child-points
            (setf points (nconc points child-points)))))
      points)))

(defun find-origin-size (coords)
  (let* (
          (xs
            (mapcar #'second coords))
          (ys
            (mapcar #'first coords))
          (min-coord
            (list (apply #'min ys) (apply #'min xs)))
          (max-coord
            (list (apply #'max ys) (apply #'max xs)))
          (size
            (mapcar #'+ (mapcar #'- max-coord min-coord) (list 1 1)))
          (origin
            (mapcar #'- min-coord)))
    (values origin size)))

;;; point: '((y x) char)
(defun collect-points (vehicles bullets)
  (let (points)
    (dolist (b bullets)
      (push (list (first b) bullet-char) points))
    (dolist (v vehicles)
      (setf points (nconc (uv-points (user-vehicle v)) points)))
    (multiple-value-bind (origin size) (find-origin-size (mapcar #'first points))
      (values
        (mapcar
          (lambda (p)
            (list
              (mapcar #'+ (first p) origin)
              (second p)))
          points)
        size))))

(defmacro plot (field width point)
  `(setf
    (elt ,field (+ (* (first (first ,point)) ,width) (second (first ,point))))
    (second ,point)))

(defun display (field field-size)
  (let ((counter 0))
    (dotimes (i (apply #'* field-size))
      (when (>= counter (second field-size))
        (setf counter 0)
        (format t "~%"))
      (format t "~a " (elt field i))
      (incf counter))))

(defun draw (vehicles bullets)
  (multiple-value-bind (points size) (collect-points vehicles bullets)
    (let ((field
            (make-array (apply #'* size) :initial-element #\Space)))
      (dolist (p points)
        (plot field (second size) p))
      (display field size))))
