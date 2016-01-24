(ql:quickload :lispbuilder-sdl)
(ql:quickload :clinch)

(defparameter world (make-array '(100 100) :element-type 'fixnum))
 
(defun init-world (world)
  (loop for i from 0 to (1- (array-dimension world 0)) do
    (loop for j from 0 to (1- (array-dimension world 1)) do
      (setf (aref world i j) (if (zerop (random 7)) 1 0)))))

(defun start ()
  (sdl:with-init ()
    (sdl:window 400 400) ; size of window
    (setf (sdl:frame-rate) 60) ; set frame-rate 60fps
    (init-world world)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()

         (sdl:update-display)))))

(start)
(exit)