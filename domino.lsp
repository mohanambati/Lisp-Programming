;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Program Name: domino.lsp
; Title: The Domino Solver.  (CSCI 4220 Assignment-2)
; Author: Mohan Sai Ambati
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
; This function controls the floww of the program.
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun main (data)
(setq current_state (list (car(car data)))) ; caliculate the current state
(setq path (find_path nil current_state)) ; call the find path function to calicualte the path from the current position.
(print_out path) ; prints the output in the prescribed format.
(exit)  ; exit from the program.
)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
; This function caliculates the path.
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun find_path (finalroute current_state) 
	(cond 
		((check-member current_state finalroute) 
		        (find_path finalroute (cdr current_state))) ;if current_state is member of path, go to other half of the domino and check the conditions
		((null (car current_state)) 
		        nil)  ;if present sate is nill then result is nill
		((equal (get_present_position current_state) '(7 6))
				(add_to_path current_state finalroute))  ;if end state add current state to final route
		((setq val              
			(remove-if #'null 
				(list 
				(find_path (add_to_path current_state finalroute) 
					(remove-if #'null 
					 (append 
					  (loop for i in data 
					    collect 
						 (find (car current_state) i :test #'check-cond2)) 
						 (find (car current_state) data :test #'check-cond1)
					 )
					 )
			    ) 
				(find_path finalroute (cdr current_state))
				)
			)
			)
			(caliculatenext_val val)
		)
	)

)

(defun get_present_position(current_state)
(append (list (car(car current_state))) (list (car (cdr(car current_state)))))
)

(defun check-cond2 (k data) 
	(and (condition1 k data) (or (condition2 k data)(equal (car data) (car k))))
)

(defun condition1 (a b)
(eq (nth 2 a) (nth 2 b))
)

(defun condition2 (a b)
(eq (nth 1 a) (nth 1 b))
)

(defun check-cond1 (i j)
(member i j :test (equal i j))
)

(defun check-member (mem1 path)
(member (car mem1) path)
)

(defun caliculatenext_val(data)
(nth (random (length val)) val)
)

(defun add_to_path (presntstate pathcovered)
(cons (car presntstate) pathcovered)
)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
; This function prints the output in the given format.
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun print_moving (path)
(loop for i in (reverse path) 
	for j in (cdr (reverse path)) 
		do 
			(format t "~%") 
			(princ "Moving from ")
			(princ "(") 
			(write (car i)) 
			(princ " ")
			(write (car (cdr i))) 
			(princ ")")  
			(princ " to ") 
			(princ "(") 
			(write (car j)) 
			(princ " ")
			(write (car (cdr j))) 
			(princ ")"))

)


(defun print_out (path)
(princ "Done, Movelist = (")
(loop for i in path 
 do  
	 (princ "(") 
	 (write (car i)) 
	 (princ " ")
	 (write (car (cdr i))) 
	 (princ ")")
)
(princ ")")
(print_moving path)
)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Program starts from here. Call the main function with the input data.
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------

(main data)

;----------------------------------------------- end of program -----------------------------------------------------------------------------------------------------