(print 'fehrjifjherik)

(defun neighbours (grid x y)
  (+ 
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
;  (if (and (= (aref grid x y) 1) (or (< live_neighbours 2) (> live_neighbours 3)))  
;    (setf (aref grid x y) 0)
   (if (or (and (= (aref grid x y) 1) (= live_neighbours (or 2 3))) (and (= (aref grid x y) 0) (= live_neighbours 3)))
    (setf (aref grid x y) 1)
    (setf (aref grid x y) 0)
   ))
 )

(defun create-table (lines columns)
  (let ((grid (make-array `(,lines ,columns))))
    (setf (aref grid 4 5) 1)
    (setf (aref grid 5 6) 1)
    (setf (aref grid 6 6) 1)
    (setf (aref grid 6 5) 1)
    (setf (aref grid 6 4) 1)
    (write grid )
    (write-line "")
      (loop for x from 1 to 8 
	do (loop for y from 1 to 8
	  do (check_neighbours grid x y)))
    (write grid )
   )
 )

(create-table 10 10)
(exit)
