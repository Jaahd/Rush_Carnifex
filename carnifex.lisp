(ql:quickload :lispbuilder-sdl)
; (ql:quickload :clinch)

(defun init ()
)

(defun main-loop ()
)

(defun clean-up ()
)

(defun start (width height)
  (sdl:with-init ()
    (sdl:window width height
        :flags sdl-cffi::sdl-opengl
        :double-buffer t
        :title-caption "Carnifex"
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

(start 800 800)
(exit)
