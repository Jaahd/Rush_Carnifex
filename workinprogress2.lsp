;(ql:quickload :lispbuilder-sdl)

; (defvar width (second sb-ext:*posix-argv*))
; (defvar height (third sb-ext:*posix-argv*))

; (defun width-height ()
;   (let ((wid 2000) (hei 2000))
; 	(sdl:with-init ()
; 				   (sdl:window wid hei :title-caption "Width and Height, from Processing.org")
; 				   (sdl:clear-display (sdl:color :r 127 :g 127 :b 127))
; 				   (loop for i from 0 to height by 2000
; 						 do (progn (sdl:draw-box (sdl:rectangle :x 0 :y i :w 200 :h 10)
; 												 :color sdl:*black*)
; 								   (sdl:draw-box (sdl:rectangle :x i :y 0 :w 10 :h 200)
; 												 :color sdl:*white*)))
; 				   (sdl:with-events ()
; 									(:quit-event () t)
; 									(:video-expose-event () (sdl:update-display))))))

(defun algo(width height)
      (format t "Width ~d Height ~d~&" width height)
 )

(defun usage()
  (format t "usage: sbcl --load game_of_life.lsp [-h] width height
positional arguments:
width width of the grid
height height of the grid
optional arguments:
-h, --help show this help message and exit~&"
    )
  (exit)
  )

(defun main (arg)
  (if (/= (list-length arg) 3)
    (usage)
    )
  (let ((width (handler-case (parse-integer (nth 1 arg)) nil)))
    (let ((height (parse-integer (nth 2 arg))))
      (if (or (<= width 0) (<= height 0))
        (usage)
        )
      (algo width height)
      )
    )
  )

(main sb-ext:*posix-argv*)
(exit)

(handler-case (parse-integer x) (error (c) (return-from get-coordinates nil) nil))
