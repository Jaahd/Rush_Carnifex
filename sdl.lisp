(ql:quickload "lispbuilder-sdl")

(defparameter *random-color* sdl:*white*)

(defun main_loop (width height)
  (setq width 1)
  (setq height 1)
)

(defun draw-win (width height)
  (sdl:with-init ()
				 (sdl:window 1080 720 :title-caption "Carniflex -- Game of Life")
				 (setf (sdl:frame-rate) 40)
				 (sdl:with-events ()
								  (:key-down-event (:key key) (when (sdl:key= key :sdl-key-escape) (exit))
										; (when (sdl:key= key :sdl-key-lshift) (exit))
;												   (when (and (sdl:key= key :sdl-key-lshift) (sdl:key= key :sdl-key-comma)) (exit))
										;sdl:draw-box (sdl:rectangle :x 100 :y 100 :w 100 :h 100) :color *random-color*))
												   (when (sdl:key= key :sdl-key-a) (exit))
												   (when (sdl:key= key :sdl-key-a) (exit))
												   (when (sdl:key= key :sdl-key-a) (exit))
												   (when (sdl:key= key :sdl-key-a) (exit))
												   (when (sdl:key= key :sdl-key-a) (exit))
												   (when (sdl:key= key :sdl-key-a) (exit)))
										;(setf (sdl:frame-rate) 5)
										;				 (sdl:clear-display sdl:*black*)
								  (:quit-event () t)
								  (:idle ()
										 (loop for y from 1 to (/ width 10) do
										 (sdl:draw-line-* () () () height)
											   ((loop for x from 1 to (/ height 10) do
													  (
													   (sdl:draw-line-* () () () ())
													   )
													  )
											   )

										 (sdl:draw-line-* 500 40 500 400)
;										 (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
;													   :color *random-color*)
										 (main_loop width height)
										 (sdl:update-display)
										 (sdl:clear-display sdl:*black*)))))
  )

(defun start (grid pos_x pos_y)
	  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)(draw-win width height))
  )

; grid_loop grid pos_x pos_y
; give_birth grid x y
