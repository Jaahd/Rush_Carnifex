(defstruct cell
              (gen 0)
              (life 0)
 )

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


(defun create-table (lines columns)
  (let ((grid (make-array `(,lines ,columns))))
    (dotimes (x lines)
      (dotimes (y columns)
        (setf (aref grid x y) (make-cell))
        )
    )
;    (setf (aref grid 4 5) (make-cell))
;    (setf (aref grid 5 6) 1)
;    (setf (aref grid 6 6) 1)
;    (setf (aref grid 6 5) 1)
;    (setf (aref grid 6 4) 1)
    (write grid )
;    (write-line "")
;    ; ajouter colones extérieurs
;    (loop for x from 1 to 8 
;          do (loop for y from 1 to 8
;                   do (check_neighbours grid x y)))
;    (write grid )
    ; fonction qui va modifier la valeur de la case et la somme des cases autour
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
