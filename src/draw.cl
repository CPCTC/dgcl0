(defun find-field-size (vehicles)
  (let*
    ((all-coords
       (let (ac)
         (dolist (v vehicles)
           (maphash
             (lambda (k val)
               (declare (ignore val))
               (push (mapcar #'+ k (slot-value v 'pos)) ac))
             (slot-value v 'grid)))
         ac))
     (min-coord
       `(
         ,(apply #'min (mapcar #'first all-coords))
         ,(apply #'min (mapcar #'second all-coords))))
     (max-coord
       `(
         ,(apply #'max (mapcar #'first all-coords))
         ,(apply #'max (mapcar #'second all-coords)))))
    (values
      (mapcar #'+ (mapcar #'- max-coord min-coord) '(1 1))
      (mapcar #'- min-coord))))

(defun draw (vehicles)
  (multiple-value-bind (field-size origin) (find-field-size vehicles)
    (let ((field (make-array (apply #'* field-size) :initial-element #\Space)))
      (dolist (vehicle vehicles)
        (maphash
          (lambda (k v)
            (let ((absolute-pos (mapcar #'+ origin (slot-value vehicle 'pos) k)))
              (setf
                (elt field (+ (* (first absolute-pos) (second field-size)) (second absolute-pos)))
                (second v))))
          (slot-value vehicle 'grid)))
      (let ((counter 0))
        (dotimes (i (apply #'* field-size))
          (when (>= counter (second field-size))
            (setf counter 0)
            (format t "~%"))
          (format t "~a " (elt field i))
          (incf counter))))))
