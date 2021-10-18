#|
                  .
                    . .
              *
                * * . .
  . .   . g .     .
. s s s s s g g * *
s s p c p s . *
. s s s s s g g
  . .   . g .
|#

(let ((c 0) (n 25)
      (sensed-id nil)
      (action-tab (let ((at (make-hash-table)))
                    (flet ((fire (dir)
                             (let ((coords
                                     (case dir
                                       (0 '(-2 2))
                                       (1 '(0 2))
                                       (2 '(2 2)))))
                               (funcall (dgcl0-driver:neighbor :up) nil
                                        :right nil :right nil :up coords)
                               (funcall (dgcl0-driver:neighbor :up) nil
                                        :right nil :right nil :right coords)
                               (funcall (dgcl0-driver:neighbor :down) nil
                                        :right nil :right nil :right coords)
                               (funcall (dgcl0-driver:neighbor :down) nil
                                        :right nil :right nil :down coords))))

                      (setf (gethash 0 at) (lambda ()       ; saw someone to top-left
                                             (funcall (dgcl0-driver:neighbor :right) :down)
                                             (fire 2)
                                             (dgcl0-driver:translate :up)
                                             (funcall (dgcl0-driver:neighbor :left) :down)))
                      (setf (gethash 1 at) (lambda ()       ; saw someone above
                                             (funcall (dgcl0-driver:neighbor :right) :left)
                                             (fire 1)
                                             (dgcl0-driver:translate :left)
                                             (funcall (dgcl0-driver:neighbor :left) :down)))
                      (setf (gethash 2 at) (lambda ()       ; saw someone top-right
                                             (fire 0)
                                             (dgcl0-driver:translate :down)
                                             (funcall (dgcl0-driver:neighbor :left) :down)))
                      (setf (gethash 3 at) (lambda ()       ; saw someone right
                                             (fire 1)
                                             (dgcl0-driver:translate :left)
                                             (funcall (dgcl0-driver:neighbor :left) :down)))
                      (setf (gethash 4 at) (lambda ()       ; saw someone bottom-right
                                             (fire 2)
                                             (dgcl0-driver:translate :up)
                                             (funcall (dgcl0-driver:neighbor :left) :down)))
                      (setf (gethash 5 at) (lambda ()       ; saw someone below
                                             (funcall (dgcl0-driver:neighbor :right) :right)
                                             (fire 1)
                                             (dgcl0-driver:translate :left)
                                             (funcall (dgcl0-driver:neighbor :left) :down)))
                      (setf (gethash 6 at) (lambda ()       ; saw someone bottom-left
                                             (funcall (dgcl0-driver:neighbor :right) :down)
                                             (fire 0)
                                             (dgcl0-driver:translate :down)
                                             (funcall (dgcl0-driver:neighbor :left) :down)))
                      (setf (gethash 7 at) (lambda ()       ; saw someone left
                                             (funcall (dgcl0-driver:neighbor :right) :down)
                                             (fire 1)
                                             (dgcl0-driver:translate :left)
                                             (funcall (dgcl0-driver:neighbor :left) :down)))
                      at))))
  (flet ((core ()
           (setf sensed-id nil)
           ; run all sensors
           (funcall (dgcl0-driver:neighbor :up) t
                    :right t :right t               ; top right
                    :down t :down t                 ; right side
                    :left t :left t :left t :left t ; bottom
                    :up t :left t :right nil :up t  ; left side
                    :right t)                       ; top left
           (if sensed-id
             (progn
               (setf c 0)
               (funcall (gethash sensed-id action-tab)))
             (progn
               #|roam|#
               (when (= c (- n 1))
                 (dgcl0-driver:rotate :down))
               (setf c (mod (1+ c) n))
               (funcall (dgcl0-driver:neighbor :left) :down)
               (funcall (dgcl0-driver:neighbor :right) :down))))
         (pivot (dir)
           (dgcl0-driver:rotate dir))
         (mk-sensor (y x id)
           (lambda (g &rest lst)
             (when g
               (let ((c (dgcl0-driver:sense y x)))
                 (unless (or (char= c #\Space) (char= c #\*))
                   (setf sensed-id id))))
             (when lst
               (apply (dgcl0-driver:neighbor (car lst)) (cdr lst)))))
         (gun (coords)
           (apply #'dgcl0-driver:shoot coords)
           (funcall (dgcl0-driver:neighbor :right) coords)))
    (dgcl0:defvehicle "Dagiron" :nodes
      ((#'core (0 0))
       (#'pivot (0 -1))
       (#'pivot (0 1))

       ((mk-sensor -1 -1 0) (-1 0))
       ((mk-sensor -1 0 1) (-1 1))
       ((mk-sensor -1 1 2) (-1 2))
       ((mk-sensor 0 1 3) (0 2))
       ((mk-sensor 1 1 4) (1 2))
       ((mk-sensor 1 0 5) (1 1))
       ((mk-sensor 1 -1 6) (1 0))
       ((mk-sensor 1 -1 6) (1 -1))
       ((mk-sensor 0 -1 7) (1 -2))
       ((mk-sensor 0 -2 7) (0 -2))
       ((mk-sensor 0 -1 7) (0 -3))
       ((mk-sensor 0 -1 7) (-1 -2))
       ((mk-sensor -1 -1 0) (-1 -1))

       (#'gun (-2 2))
       (#'gun (-1 3))
       (#'gun (-1 4))

       (#'gun (2 2))
       (#'gun (1 3))
       (#'gun (1 4))))))
