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

(defun print_grid_life (grid pos_x pos_y) "TO EREASE ------ display function"
  (write-line "life")
  (dotimes (x pos_x)
    (dotimes (y pos_y)
      (write (cell-life (aref grid x y)))
      (format t " ")
      )
    (write-line "")
    )
  )

(defun print_grid_gen (grid pos_x pos_y)
  (write-line "gen")
  (dotimes (x pos_x)
    (dotimes (y pos_y)
      (write (cell-gen (aref grid x y)))
      (format t " ")
      )
    (write-line "")
    )
  )

(defun swap_life_gen (grid pos_x pos_y) "'swap gen and life' function"
  (write-line "swap")
  (dotimes (x pos_x)
    (dotimes (y pos_y)
      (if (or (= (cell-life (aref grid x y)) 3) (= (cell-life (aref grid x y)) 4))
        (setf (cell-gen (aref grid x y)) 1)
        (setf (cell-gen (aref grid x y)) 0)
        ))
    )
  )

(defun create-table (pos_x pos_y) "grid ceation and initialization function"
  (let ((grid (make-array `(,pos_x ,pos_y))))
    (dotimes (x pos_x)
      (dotimes (y pos_y)
        (setf (aref grid x y) (make-cell))
        )
      )
    (setf (cell-gen (aref grid 4 5)) 1)
    (setf (cell-gen (aref grid 5 6)) 1)
    (setf (cell-gen (aref grid 6 6)) 1)
    (setf (cell-gen (aref grid 6 5)) 1)
    (setf (cell-gen (aref grid 6 4)) 1)

; a virer --v
    (print_grid_gen grid pos_x pos_y)
    (print_grid_life grid pos_x pos_y)

; a mettre dans une autre fonction ---v
    (write-line "check")
    (loop for x from 0 to (- pos_x 1)
          do (loop for y from 0 to (- pos_y 1)
                   do (check_neighbours grid x y (- pos_x 1) (- pos_y 1))
                   )
          )

; a virer ---v
    (print_grid_gen grid pos_x pos_y)
    (print_grid_life grid pos_x pos_y)
    (swap_life_gen grid pos_x pos_y)
    (print_grid_gen grid pos_x pos_y)
    (print_grid_life grid pos_x pos_y)
    )
  )

(create-table 10 10)
(exit)

; gen . live
; gen = 1 ou 0
; live = 0+ (= live_neighbours)
; t0 : grille de départ avec gen = live
; t1 : live = live_neighbours
; t2 : gen = 1 ou 0 en fonction de live


; x = pos_x = pos_y
; y = pos_y = pos_x
