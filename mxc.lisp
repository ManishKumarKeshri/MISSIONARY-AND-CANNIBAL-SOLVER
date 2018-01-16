;;Missionary cannibal problem
; Name: Manish Kumar Keshri
; Date: 12/12/2017
; steps to run:
; 1) start clisp
; 2) Load the lisp file as (load "path\mxc.lisp")
; 3) call solution function as (solution no_of_missionary no_of_cannibal boat_size)

;FUNCTIONS:
; solution: main function to intialise and sart problem solving
; print path: to print the movements
; m_c_sol: to solve the problem
; sort states: to sort the queueu
; partition: helps in sorting--> quick sort
; isgoal: checks whether the given state is a goal state or not
; isvalid: to check the vaildidty of newly generated states

(defstruct s_n ;sturcture to store attrbutes of a state like cost function, state_id,parent_id 
	state ; puzzle elements array
	s_id  ; state_id to each state
	p_s_id ;state_id of the parent state
	h    ; heuristic value
	g    ; path cost
	f	 ; cost function i.e. heuristic value + path cost
	)
(setf all_states (make-array 0 :fill-pointer t :adjustable t)) ; stores the the generated states
(setf visited_states (make-array 0 :fill-pointer t :adjustable t)) ; stores all states which have been vsisited
(setf path (make-array 0 :fill-pointer t :adjustable t)) ; stores the sequence of states which reaches to the goal state
(defun solution(nm nc s) ;Main function to start solving the problem
	(setf i_s (make-array 3)) ; intial state array to store intial values of missionaries and cannibals and boat postion
	(setf (aref i_s 0) nm)    
	(setf (aref i_s 1) nc)
	(setf (aref i_s 2) 1)
	(vector-push-extend (make-s_n			; create the initial state
				:state i_s
				:s_id 0
				:p_s_id 0
				:h (/ (+ nm nc) s)
				:g 0
				:f (/ (+ nm nc) s)
			)all_states
	)                               
	(m_c_sol nm nc s)  ; call fuction to solve the problem
	(setq i (1- (length visited_states)))
	(vector-push-extend (s_n-state (aref visited_states i)) path)
	(setq c_s_parent (s_n-p_s_id (aref visited_states i)))   ;to find the states that led to solution
	(loop
		(if(= 0 i)
			(return)
		)
		(loop
			(if (= c_s_parent (s_n-s_id (aref visited_states i)))
				(progn
					(vector-push-extend (s_n-state (aref visited_states i)) path) 
					(setq c_s_parent (s_n-p_s_id (aref visited_states i)))
				(return)
				)
				(decf i)
			)
		)
	)
	(setf lm (vector-pop path))  ; the last move
	(print_path)
)
(defun print_path() ;to print the movements
	(if (= (length path) 0)
		(format t "~%~%successfully transferred all missionariers and cannibals from left to right bank")
		(progn
			(let((c_m (vector-pop path))) ;pop from path to display the states which led to the goal state
				(if (= (aref c_m 2) 0)
					(format t "~%Left Bank Intially(M C):(~S ~S) Left Bank after boat left(M C):(~S ~S)   Boat carrying from left bank to right(M C):(~S ~S)      Right Bank Intially(M C):(~S ~S)  Right Bank after boat arrived(M C):(~S ~S)"
						(aref lm 0) (aref lm 1) (aref c_m 0) (aref c_m 1) (- (aref lm 0) (aref c_m 0)) (- (aref lm 1) (aref c_m 1))
						(- (aref i_s 0) (aref lm 0)) (- (aref i_s 1) (aref lm 1))
						(- (aref i_s 0) (aref c_m 0))
						(- (aref i_s 1) (aref c_m 1)) 					
					)
					(format t "~%Left Bank after boat arrived(M C):(~S ~S)  Left Bank Intially(M C):(~S ~S)  Boat carrying from right bank to left(M C):(~S ~S)    Right Bank after boat left(M C):(~S ~S)  Right Bank Intially(M C):(~S ~S)"
						(aref c_m 0) (aref c_m 1)
						(aref lm 0)
						(aref lm 1)
						(- (aref c_m 0) (aref lm 0))
						(- (aref c_m 1) (aref lm 1))
						(- (aref i_s 0) (aref c_m 0))
						(- (aref i_s 1) (aref c_m 1))
						(- (aref i_s 0) (aref lm 0))
						(- (aref i_s 1) (aref lm 1))
					)
				)
				(setf lm c_m) ;setf current move as last move
			)
			(print_path)		;recursive calling the print the states
		)	
	)
)
(setf arr_boat (make-array 3))
(setf count 0)
(defun m_c_sol(nm nc s) ;to solve the problem
	(setf c_s(vector-pop all_states))
	(vector-push-extend c_s visited_states)
	(if (/= 0 (isgoal c_s))
	0
		(progn
			(loop for i from 0 to s do
				(loop for j from 0 to s do
					(if(and(and(< (+ i j) (1+ s)) (> (+ i j) 0)) (or (<= j i) (= 0 i) (= 0 j)))
						(progn
							(setf (aref arr_boat 0) i)
							(setf (aref arr_boat 1) j)
							(setf (aref arr_boat 2) 1)
							(setf n_s (make-array (length (s_n-state c_s))))  ;generating boat state
							(setf heuristic 0)
							(loop for i from 0 to (1- (length (s_n-state c_s))) do ;generating new state
								(if(= (aref (s_n-state c_s) 2) 1)    
									(progn
										(setf (aref n_s i) (- (aref (s_n-state c_s) i)(aref arr_boat i)))
									)
									(progn
										(setf (aref n_s i) (+ (aref (s_n-state c_s) i)(aref arr_boat i)))
									)
								)
							)
							(setf heuristic (/ (+ (aref n_s 0) (aref n_s 1)) s))  ; getting the heuristic value
							(if(and (= 1 (is_valid n_s nm)) (= 1 (isvisited n_s))) ; chceking the validing of the state and also checking if it has been visisted or not
								(progn
									(vector-push-extend (make-s_n ;pushing the current state to the open states
										:state n_s
										:s_id (incf count)
										:p_s_id (s_n-s_id c_s)
										:h heuristic
										:g (+ 1 (s_n-g c_s))
										:f (+ (+ 1 (s_n-g c_s)) heuristic)
												)all_states
									)
								)
							)
						)
					)
				)
			)
		(sort_states 0 (1- (length all_states))) ; sort the queue conataining all states
		(m_c_sol nm nc s) ;recursive call to solve to the problem 
		)
	)
)
(defun sort_states (low high)   ; sorting the queue with quick sort
	(if (< low high)
		(progn
			(setf p (partition low high))
			(sort_states low (- p 1))
			(sort_states (+ p 1) high)
		)
	)	
)
(defun partition (low high) ; partition the queue to help in quick sort
	(setf pivot (s_n-f (aref all_states high)))
	(setf i (- low 1))
	(loop for j from low to (- high 1) do
		(if (>= (s_n-f (aref all_states j)) pivot)
			(progn
				(incf i)
				(rotatef (aref all_states i) (aref all_states j))
			)
		)
	)
	(rotatef (aref all_states (+ 1 i)) (aref all_states high))
	(incf i)
	i
)
(defun isvisited (n_s)   ; check if the state has been visited or not
	(loop for i from 0 to (1- (length visited_states)) do
		(if(equalp n_s (s_n-state (aref visited_states i)))
	    		(return-from isvisited 0)
		)
	)
	1
)
(defun isgoal (curr_state) ;check if the given state is the goal state or not
	(setf goal_state '#(0 0 0))
	(if (equalp(s_n-state curr_state) goal_state)
	1 ; return 1 if it is a goal state
	0
	)
)

(defun is_valid(ns nm) ; checking the vailidity of the state
	(if (or (> (aref ns 0) nm) (> (aref ns 1) nm) (< (aref ns 0) 0) (< (aref ns 1) 0))
		0
		(if (and (> (aref ns 1) (aref ns 0)) (> (aref ns 1) 0) 	(> (aref ns 0) 0))
				0
				(if (or (> (- nm (aref ns 1)) nm) (> (- nm (aref ns 0)) nm) (< (- nm (aref ns 1)) 0) (< (- nm (aref ns 0)) 0))
					0
					(if (and (> (- nm (aref ns 1)) 0)  (> (- nm (aref ns 0)) 0)  (> (- nm (aref ns 1)) (- nm (aref ns 0))))
						0
						1	; return 1 if all the conditions are passed and this is a valid state
					)
				)
		)
	)
)	


