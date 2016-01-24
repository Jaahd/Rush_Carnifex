(defstruct cell (gen 0) (life 0))

; couper en 2 fonction et ajouter un si x =0 ou y = 0 ou x = width ou y = height
(defun neighbours (grid x y)
  (+ 
    (aref grid x  y) 
    (aref grid (+ x 1) y) 
    (aref grid (- x 1) y) 
    (aref grid x (+ y 1)) 
    (aref grid x (- y 1)) 
    (aref grid (+ x 1) (+ y 1)) 
    (aref grid (- x 1) (- y 1)) 
    (aref grid (+ x 1) (- y 1)) 
    (aref grid (- x 1) (+ y 1))
    )
  )

(defun check_neighbours (grid x y)
  (let ((live_neighbours (neighbours grid x y)))
    ; fonction qui modifier live en fonction de live_neighbours
    (if (or (and (= (aref grid x y) 1) (= live_neighbours (or 2 3))) (and (= (aref grid x y) 0) (= live_neighbours 3)))
      (setf (aref grid x y) 1)
      (setf (aref grid x y) 0)
      ))
  )

(defun print_grid_life (grid height width)
  (write-line "life")
  (dotimes (x width)
    (dotimes (y height)
      (write (cell-life (aref grid x y)))
      (format t " ")
      )
    (write-line "")
    )
  )

(defun print_grid_gen (grid height width)
  (write-line "gen")
  (dotimes (x width)
    (dotimes (y height)
      (write (cell-gen (aref grid x y)))
      (format t " ")
      )
    (write-line "")
    )
  )

(defun swap_life_gen (grid height width)
  (write-line "swap")
  (dotimes (x width)
    (dotimes (y height)
      (setf (cell-gen (aref grid x y)) (cell-life (aref grid x y)))
      )
    )
  )

(defun create-table (lines columns)
  (let ((grid (make-array `(,lines ,columns))))
    (dotimes (x lines)
      (dotimes (y columns)
        (setf (aref grid x y) (make-cell))
        )
      )
    (setf (cell-gen (aref grid 4 5)) 1)
    (setf (cell-gen (aref grid 5 6)) 1)
    (setf (cell-gen (aref grid 6 6)) 1)
    (setf (cell-gen (aref grid 6 5)) 1)
    (setf (cell-gen (aref grid 6 4)) 1)

    (setf (cell-life (aref grid 9 9)) 9)
    (print_grid_gen grid lines columns)
    (print_grid_life grid lines columns)
    (write-line "")
    ; ajouter colones extérieurs
    ;    (loop for x from 1 to (- columns 1)
    ;          do (loop for y from 1 to (- lines 1)
    ;                   do (check_neighbours grid x y)))
    (print_grid_gen grid lines columns)
    (print_grid_life grid lines columns)
    (swap_life_gen grid lines columns)
    (print_grid_gen grid lines columns)
    (print_grid_life grid lines columns)
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


; x = width = columns
; y = height = lines
