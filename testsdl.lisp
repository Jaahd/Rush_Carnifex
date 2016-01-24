(ql:quickload :lispbuilder-sdl)
(ql:quickload :clinch)

(defvar viewport)
(defvar projection-matrix)

(defvar triangle)
(defvar triangle-point-buffer)
(defvar triangle-indices-buffer)

(defun init ()
  (setf viewport (make-instance 'clinch:viewport))

  (setf triangle-point-buffer
    (make-instance 'clinch:buffer
           :Stride 3
           :data '(0.0  100.0 -1.0
                   -100.0 -100.0 -1.0
                   100.0 -100.0 -1.0)
    )
  )

  (setf triangle-indices-buffer
    (make-instance 'clinch:buffer
               :qtype
               :unsigned-int
               :target
               :element-array-buffer
               :Stride 1
               :data '(0 1 2)
    )
  )

  (setf triangle
    (make-instance 'clinch:entity
               :indexes triangle-indices-buffer 
               :values `((:vertices ,triangle-point-buffer))
    )
  )
)

(defun main-loop ()
  (gl:clear-color 0.0 0.0 1.0 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render triangle)
)

(defun clean-up ()
  (clinch:unload triangle-point-buffer)
  (clinch:unload triangle-indices-buffer)
)

(defun start ()
  (sdl:with-init ()
    (sdl:window 400 300
        :flags sdl-cffi::sdl-opengl
        :double-buffer t
        :title-caption "Tutorial 1"
    )
    (init)
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
         (main-loop)
         (sdl:update-display)
      )
    )
    (clean-up)
  )
)

(start)
(exit)
