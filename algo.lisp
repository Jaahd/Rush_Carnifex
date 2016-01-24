(defstruct cell (gen 0) (life 0))

(defun neighbours (grid x y pos_x pos_y) "'add neighbours value with correct conditions' function"
  (cond ((and (= x  0) (= y 0)) (+
                                  (cell-gen (aref grid x  y))
                                  (cell-gen (aref grid (+ x 1) y))
                                  (cell-gen (aref grid x (+ y 1)))
                                  (cell-gen (aref grid (+ x 1) (+ y 1)))
                                  ))
        ((and (= x  pos_x) (= y pos_y)) (+
                                          (cell-gen (aref grid x  y))
                                          (cell-gen (aref grid (- x 1) y))
                                          (cell-gen (aref grid x (- y 1)))
                                          (cell-gen (aref grid (- x 1) (- y 1)))
                                          ))
        ((and (= x 0) (> y 0) (< y pos_y)) (+
                                             (cell-gen (aref grid x  y))
                                             (cell-gen (aref grid (+ x 1) y))
                                             (cell-gen (aref grid x (+ y 1)))
                                             (cell-gen (aref grid x (- y 1)))
                                             (cell-gen (aref grid (+ x 1) (+ y 1)))
                                             (cell-gen (aref grid (+ x 1) (- y 1)))
                                             ))
        ((and (= x 0) (= y pos_y)) (+
                                     (cell-gen (aref grid x  y))
                                     (cell-gen (aref grid (+ x 1) y))
                                     (cell-gen (aref grid x (- y 1)))
                                     (cell-gen (aref grid (+ x 1) (- y 1)))
                                     ))
        ((and (= y 0) (> x 0) (< x pos_x)) (+
                                             (cell-gen (aref grid x  y))
                                             (cell-gen (aref grid (+ x 1) y))
                                             (cell-gen (aref grid (- x 1) y))
                                             (cell-gen (aref grid x (+ y 1)))
                                             (cell-gen (aref grid (+ x 1) (+ y 1)))
                                             (cell-gen (aref grid (- x 1) (+ y 1)))
                                             ))
        ((and (= y 0) (= x pos_x)) (+
                                     (cell-gen (aref grid x  y))
                                     (cell-gen (aref grid (- x 1) y))
                                     (cell-gen (aref grid x (+ y 1)))
                                     (cell-gen (aref grid (- x 1) (+ y 1)))
                                     ))
        ((and (= x  pos_x) (> y 0) (< y pos_y)) (+
                                                  (cell-gen (aref grid x  y))
                                                  (cell-gen (aref grid (- x 1) y))
                                                  (cell-gen (aref grid x (+ y 1)))
                                                  (cell-gen (aref grid x (- y 1)))
                                                  (cell-gen (aref grid (- x 1) (- y 1)))
                                                  (cell-gen (aref grid (- x 1) (+ y 1)))
                                                  ))
        ((and (= y  pos_y) (> x 0) (< x pos_x)) (+
                                                  (cell-gen (aref grid x  y))
                                                  (cell-gen (aref grid (+ x 1) y))
                                                  (cell-gen (aref grid (- x 1) y))
                                                  (cell-gen (aref grid x (- y 1)))
                                                  (cell-gen (aref grid (- x 1) (- y 1)))
                                                  (cell-gen (aref grid (+ x 1) (- y 1)))
                                                  ))
        (t (+
             (cell-gen (aref grid x  y))
             (cell-gen (aref grid (+ x 1) y))
             (cell-gen (aref grid (- x 1) y))
             (cell-gen (aref grid x (+ y 1)))
             (cell-gen (aref grid x (- y 1)))
             (cell-gen (aref grid (+ x 1) (+ y 1)))
             (cell-gen (aref grid (- x 1) (- y 1)))
             (cell-gen (aref grid (+ x 1) (- y 1)))
             (cell-gen (aref grid (- x 1) (+ y 1)))
             ))
        )
  )

(defun check_neighbours (grid x y pos_x pos_y) "set 'neighbours' return in cell-life"
  (setf (cell-life (aref grid x y)) (neighbours grid x y pos_x pos_y))
  )

; (defun print_grid_life (grid pos_x pos_y) "TO EREASE ------ display function"
;   (write-line "life")
;   (dotimes (x pos_x)
;     (dotimes (y pos_y)
;       (write (cell-life (aref grid x y)))
;       (format t " ")
;       )
;     (write-line "")
;     )
;   )
; 
; (defun print_grid_gen (grid pos_x pos_y)
;   (write-line "gen")
;   (dotimes (x pos_x)
;     (dotimes (y pos_y)
;       (write (cell-gen (aref grid x y)))
;       (format t " ")
;       )
;     (write-line "")
;     )
;   )

(defun swap_life_gen (grid pos_x pos_y) "'swap gen and life' function"
  (dotimes (x pos_x)
    (dotimes (y pos_y)
      (if (or (and (= (cell-gen (aref grid x y)) 0) (= (cell-life (aref grid x y)) 3))
            (and (= (cell-gen (aref grid x y)) 1) (or (= (cell-life (aref grid x y)) 3) (= (cell-life (aref grid x y)) 4))))
        (setf (cell-gen (aref grid x y)) 1)
        (setf (cell-gen (aref grid x y)) 0)
        ))
    )
  )

(defun grid_loop (grid pos_x pos_y) ""
  (loop for x from 0 to (- pos_x 1)
        do (loop for y from 0 to (- pos_y 1)
                 do (check_neighbours grid x y (- pos_x 1) (- pos_y 1))
                 )
        )
  (swap_life_gen grid pos_x pos_y)
  )

(defun give_birth (grid x y)
  (if (= (cell-gen (aref grid x y)) 0)
  (setf (cell-gen (aref grid x y)) 1)
  (setf (cell-gen (aref grid x y)) 0)
  )
  )

(defun reset_grid (grid pos_x pos_y)
    (dotimes (x pos_x)
      (dotimes (y pos_y)
        (setf (cell-gen (aref grid x y)) 0)
        (setf (cell-life (aref grid x y)) 0)
        )
      )
  )

; gen . live
; gen = 1 ou 0
; live = 0+ (= live_neighbours)
; t0 : grille de départ avec gen = live
; t1 : live = live_neighbours
; t2 : gen = 1 ou 0 en fonction de live


; x = pos_x = pos_y
; y = pos_y = pos_x

(ql:quickload "lispbuilder-sdl")

(defparameter *random-color* sdl:*white*)

(defun main_loop (grid width height sl_time p_time)
  (if p_time
    (grid_loop grid width height)
    )
  (sleep sl_time)
  )

(defun draw-win (grid width height)
  (let ((sl_time 0.1) (p_time nil))
  (sdl:with-init ()
                 (sdl:window 1080 720 :title-caption "Carniflex -- Game of Life")
                 (setf (sdl:frame-rate) 40)
                 (sdl:with-events ()
                                  (:key-down-event (:key key :mod mod)
                                                   (when (sdl:key= key :sdl-key-escape) (exit))
                                                   (when (and (or (= mod 1) (= mod 2)) (sdl:key= key :sdl-key-comma)) (setf sl_time (+ sl_time 0.1)))
                                                   (when (and (or (= mod 1) (= mod 2)) (sdl:key= key :sdl-key-period)) (if (< (- sl_time 0.1) 0.1) (setf sl_time 0.1) (setf sl_time (- sl_time 0.1))))
                                          ;         (when (sdl:key= key :sdl-key-w) (exit))
                                          ;         (when (sdl:key= key :sdl-key-a) (exit))
                                          ;         (when (sdl:key= key :sdl-key-s) (exit))
                                          ;         (when (sdl:key= key :sdl-key-d) (exit))
                                                   (when (sdl:key= key :sdl-key-p) (if (null p_time) (setf p_time t) (setf p_time nil)))
                                                   (when (sdl:key= key :sdl-key-r) (reset_grid grid width height))
                                          ;         (when (sdl:key= key :sdl-key-minus) (exit))
                                          ;         (when (and (or (= mod 1) (= mod 2)) (sdl:key= key :sdl-key-equals)) (exit))
                                                   )
                                  (:mouse-button-up-event (:button button :x x :y y)
                                                          (format t "bu:~a x:~a y:~a ~%" button x y)
                                                          )

                                  ;(setf (sdl:frame-rate) 5)
                                  ;				 (sdl:clear-display sdl:*black*)
                                  (:quit-event () t)
                                  (:idle ()
                                         ;										 (loop for y from 1 to (/ width 10) do
                                         ;										 (sdl:draw-line-* () () () height)
                                         ;											   ((loop for x from 1 to (/ height 10) do
                                         ;													  (
                                         ;													   (sdl:draw-line-* () () () ())
                                         ;													   )
                                         ;													  )
                                         ;											   )

                                         ;										 (sdl:draw-line-* 500 40 500 400)
                                         ;										 (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
                                         ;													   :color *random-color*)
                                         (main_loop grid width height sl_time p_time)
                                         (sdl:update-display)
                                         (sdl:clear-display sdl:*black*)
                                         )
                                  )
                 )
  )
  )

(defun start (grid pos_x pos_y)
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)(draw-win grid pos_x pos_y))
  )

; grid_loop grid pos_x pos_y
; give_birth grid x y

(defun create-grid (pos_x pos_y) "grid ceation and initialization function"
  (let ((grid (make-array `(,pos_x ,pos_y))))
    (dotimes (x pos_x)
      (dotimes (y pos_y)
        (setf (aref grid x y) (make-cell))
        )
      )
;    (give_birth grid 4 5)
;    (setf (cell-gen (aref grid 5 6)) 1)
;    (setf (cell-gen (aref grid 6 6)) 1)
;    (setf (cell-gen (aref grid 6 5)) 1)
;    (setf (cell-gen (aref grid 6 4)) 1)

(start grid pos_x pos_y)

    ; a virer --v
;    (print_grid_gen grid pos_x pos_y)
;    (print_grid_life grid pos_x pos_y)

;    (loop for aaaa from 0 to 5
;          do (grid_loop grid pos_x pos_y)
;          do (print_grid_gen grid pos_x pos_y)
;          do (print_grid_life grid pos_x pos_y)
;          )
    ; fonction qui creer la fenetre et tout
    ;(start grid pos_x pos_y)
    ; a virer ---v
;    (print_grid_gen grid pos_x pos_y)
;    (print_grid_life grid pos_x pos_y)
    )
  )

(defun usage()
  (format t "usage: sbcl --load game_of_life.lsp [-h] width height

positional arguments:
  width                  width of the grid

  height                 height of the grid

optional arguments:
  -h, --help             show this help message and exit~&"
    )
  (exit)
  )

(defun get_arg (arg)
  (let ((pos_y (parse-integer (nth 1 arg))))
    (let ((pos_x (parse-integer (nth 2 arg))))
      (if (or (<= pos_x 0) (<= pos_y 0))
        (usage)
        (create-grid pos_x pos_y)
        )
      )
    )
  )


(defun main (arg)
  (if (/= (list-length arg) 3)
    (usage)
    (get_arg arg)
    )
  )

(main sb-ext:*posix-argv*)
(exit)
