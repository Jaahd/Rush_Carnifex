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

(ql:quickload "lispbuilder-sdl")

(defparameter *random-color* sdl:*cyan*)

(defun main_loop (grid width height sl_time p_time)
  (loop for i from 0 to (- width 1) do
        (loop for j from 0 to (- height 1) do
              (if (= (cell-gen (aref grid i j)) 1)
                (sdl:draw-box (sdl:rectangle :x (floor (* 1200 (/ i width))) :y (floor (* 1200 (/ j height))) :w (floor (/ 1200 width)) :h (floor (/ 1200 height))) :color (sdl:color :r 100 :g 100 :b 100))
                (sdl:draw-box (sdl:rectangle :x (floor (* 1200 (/ i width))) :y (floor (* 1200 (/ j height))) :w (floor (/ 1200 width)) :h (floor (/ 1200 height))) :color sdl:*black*)
                )
              )
        )
  (if p_time
    (progn (sleep sl_time) (grid_loop grid width height))
    )
  )

(defun draw-win (grid width height)
  (let ((sl_time 0.1) (p_time nil))
    (sdl:with-init ()
                   (sdl:window 1200 1200 :title-caption "Carniflex -- Game of Life")
                   (setf (sdl:frame-rate) 40)
                   (loop for k from 1 to width do
                         (sdl:draw-line-* (floor (* 1200 (/ k width))) 0 (floor (* 1200 (/ k width)))  1200 :color *random-color*))
                   (loop for l from 1 to height do
                         (sdl:draw-line-* 0 (floor (* 1200 (/ l height))) 1200 (floor (* 1200 (/ l height))) :color *random-color*))
                   (sdl:with-events ()
                                    (:key-down-event (:key key :mod mod)
                                                     (when (sdl:key= key :sdl-key-escape) (exit))
                                                     (when (and (or (= mod 1) (= mod 2)) (sdl:key= key :sdl-key-comma)) (setf sl_time (+ sl_time 0.1)))
                                                     (when (and (or (= mod 1) (= mod 2)) (sdl:key= key :sdl-key-period)) (if (< (- sl_time 0.1) 0.1) (setf sl_time 0.1) (setf sl_time (- sl_time 0.1))))
                                                     (when (sdl:key= key :sdl-key-p) (if (null p_time) (setf p_time t) (setf p_time nil)))
                                                     (when (sdl:key= key :sdl-key-r) (reset_grid grid width height) (setf p_time nil))
                                                     )
                                    (:mouse-button-up-event (:button button :x mouse-x :y mouse-y)
                                                            (if (= button 1)
                                                              (progn (sdl:draw-box (sdl:rectangle :x (* (floor (/ mouse-x (floor (/ 1200 width)))) (floor (/ 1200 width)))
                                                                                                  :y (* (floor (/ mouse-y (floor (/ 1200 height )))) (floor (/ 1200 height )))
                                                                                                  :w (floor (/ 1200 width))
                                                                                                  :h (floor (/ 1200 height)))
                                                                                   :color *random-color*)
                                                                     (give_birth grid (floor (/ mouse-x (floor (/ 1200 width)))) (floor (/ mouse-y (floor (/ 1200 height )))))
                                                                     )
                                                              (format t "~&~d-- MOUSE-X -- ~d~&-- MOUSE-Y -- ~d~&" button mouse-x mouse-y)
                                                              ))
                                    (:quit-event () t)
                                    (:idle ()
                                           (main_loop grid width height sl_time p_time)
                                           (sdl:update-display)
                                           )
                                    )
                   )
    )
  )

(defun start (grid pos_x pos_y)
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)(draw-win grid pos_x pos_y))
  )

(defun create-grid (pos_x pos_y) "grid ceation and initialization function"
  (let ((grid (make-array `(,pos_x ,pos_y))))
    (dotimes (x pos_x)
      (dotimes (y pos_y)
        (setf (aref grid x y) (make-cell))
        )
      )
    (start grid pos_x pos_y)
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
