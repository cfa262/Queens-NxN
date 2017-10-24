;; Calvin Alvarez
;; calvare6
;; G00982843
;; HW2
;; CS480

(defun wrapper()

	(print '+++++1+++++)
	(print (list 'checking 'board 'legality))
	(print (list 'list '1 '2 '2 '4)) (checkLegalBoard (list 1 2 2 4))
	(print (list 'list '1 '2 '3 '4)) (checkLegalBoard (list 1 2 3 4))
	(print (list 'list '2 '4 '1 '3)) (checkLegalBoard (list 2 4 1 3))
	(print (list 'list '1 '2 '5 '4 '5)) (checkLegalBoard (list 1 2 5 4 5))
	(print (list 'list '1 '2 '3 '4 '5)) (checkLegalBoard (list 1 2 3 4 5))	
	(print (list 'list '2 '4 '1 '3 '5)) (checkLegalBoard (list 2 4 1 3 5))
	
	
	(print '+++++2+++++)
	
	(print (list 'working 'permutations))
	
	(print (list 'n '= '4)) (permutations 4)
		
	(print (list 'n '= '5)) (permutations 5)

	(print (list 'n '= '6)) (permutations 6)

	(print (list 'n '= '7)) (permutations 7)
	
	(print (list 'n '= '8)) (permutations 8)

	
	(print '+++++3+++++)
	
	(print (list 'random 'permutation 'shuffling))

	(print (list 'n '= '4)) (shuffle 4)
	(print (list 'n '= '5)) (shuffle 5)
	(print (list 'n '= '6)) (shuffle 6)
	(print (list 'n '= '7)) (shuffle 7)
	(print (list 'n '= '8)) (shuffle 8)

	(values))

;; Part 1)
;; You can write a function that checks if a board is
;; legal. A legal board configuration cannot have two queens in
;; the same column or diagonal.

;;checks Board legality
(defun checkLegalBoard(lst)

	(cond ((= 0 (checkCol lst))
		(print (list 'Column 'check 'fails. 'Board 'not 'legal)))
		(t 
		(print (list 'All 'Columns 'okay))
			(cond ((= 0 (checkDiag lst))
			(print (list 'Diagonal 'check 'fails. 'Board 'not 'legal)))
			(t 
			(print (list 'All 'diagonals 'okay))
			(print(list 'Board 'is 'Legal!)))
			)
		)
	)
	(values)
)

;;Checks column legality
(defun checkCol(ls)

	(setf tempLst ls)
	(setf checker 1)

		(loop
			(setf ci (car tempLst))

			(dolist (cj (cdr tempLst))

				(if(eql ci cj)
					(setf checker 0))
			)

			(setf tempLst (cdr tempLst))

		(when (eql tempLst nil)(return))
		)
	
	(cond ((= 1 checker)
		1)
	(t 0)
	)
)

;;checks diagonal legality
(defun checkDiag (lst)

	(setf iLst lst)
	(setf jLst (cdr lst))
	(setf checker 1)

	(let ((i 1))

		(dolist (ci iLst)

			(let ((j (+ i 1)))
				(dolist (cj jLst)

					(setf x (abs(- cj ci)))
					(setf y (abs(- i j)))

					(if (eql x y)
						(setf checker 0))

					(setq j (+ j 1))
				)
			)
			(setq i (+ i 1))
			(setf jLst (cdr jLst))
		)
	)

	(cond ((= 1 checker)
		1)
	(t 0)
	)
)

;;Part 2)
;; Generate configurations that correspond to permutations of 1..n.

;;For n >= 4
;; n = 4 => 2 working permutation combinations
;; n = 5 => 10 working permutation combinations
;; n = 6 => 4 working permutation combinations
;; n = 7 => 40 working permutation combinations
;; n = 8 => 92 working permutation combinations
;; n = 9 => 352 working permutation combinations
;; n = 10 => 724 working permutation combinations
;; For n > 10, program wouldn't handle this to find working perm combinations.
(defun permutations (n)

	(setf first_list (list))
	(setf developingList (list))
	(setf doPerm_list (list))

	(cond ((= n 1)
		(print n))
		(t

		(let ((final_list (list)))
		
			(let ((perm_list (list '(2 1) '(1 2))))

			;;loops n times for each n groups of permuations
			(loop for i from 3 upto n do

				(setf tempPerm_list perm_list)
				(setf perm_list (list))

				(dolist (x tempPerm_list)
					;(print x)
					(setf doPerm_list x)
					;(print doPerm_list)
					(loop for j from 0 upto (length doPerm_list) do

						(setf first_list (subseq doPerm_list 0 j))
						(setf last_list (subseq doPerm_list j))

						;;appends first_list, then i, then last_list
						(setf developingList first_list)
						(setf developingList (append developingList (list i)))
						(setf developingList (append developingList last_list))
						;(print developingList)	

						(setf perm_list (append perm_list (list developingList)))
						;(print perm_list)
					)
				)
			)
			(setf final_list (append final_list perm_list))
			(print (list (length final_list) 'total 'permutations))
			
			)

			(let ((count 0))
				(dolist (perm final_list)

					;(print perm)
					;;checks if legal
					(cond ( (= 1 (checkDiag perm))
						(setq count (+ count 1)))

					)
				)
				(print (list count 'working 'permutation 'combinations))
			)
			)
			)
		;final_list
		)
		(values)
)
	


;; Part 3)
;; In this part you will generate random permutations of n queens and
;; check if they correspond to legal boards.

;; Different answers for different runs..
;; n = 4, first run ==> 12 shuffles, second run ==> 1 shuffle
;; n = 5, first run ==> 6 shuffles, second run ==> 2 shuffles
;; n = 6, first run ==> 335 shuffles, second run ==> 171 shuffles
;; n = 7, first run ==> 168 shuffles, second run ==> 157 shuffles
;; n = 8, first run ==> 304 shuffles, second run ==> 83 shuffles
;; n = 9, first run ==> 234 shuffles, second run ==> 291 shuffles
;; n = 10, first run ==> 2285 shuffles, second run ==> 3498 shuffles
;; program can only handle for n <= 10.
;; n > 10 takes a very very long time.

(defun shuffle (n)

	;;will shuffle and pick one list element from permutations
	;;will stop until a permutation solution is found

	(setf first_list (list))
	(setf developingList (list))
	(setf doPerm_list (list))
	(cond ((= n 1)
		(print n))
		(t

		(let ((final_list (list)))
		
			(let ((perm_list (list '(2 1) '(1 2))))

			;;loops n times for each n groups of permuations
			(loop for i from 3 upto n do

				(setf tempPerm_list perm_list)
				(setf perm_list (list))

				(dolist (x tempPerm_list)

					;(print x)
					(setf doPerm_list x)
					;(print doPerm_list)
				
					(loop for j from 0 upto (length doPerm_list) do

						(setf first_list (subseq doPerm_list 0 j))
						(setf last_list (subseq doPerm_list j))

						;;appends first_list, then i, then last_list
						(setf developingList first_list)
						(setf developingList (append developingList (list i)))
						(setf developingList (append developingList last_list))
						;(print developingList)	

						(setf perm_list (append perm_list (list developingList)))
						;(print perm_list)
					)
				
				)
			)
			(setf final_list (append final_list perm_list))
			;(print (length final_list))
			
			)

			(setf fSize (length final_list)) 
			(let ((new_list final_list))
			(let ((count 0))

			(loop

				(setf temp_list (nth (random (length new_list)) new_list))
				;(print temp_list)
				(setq count (+ count 1))

				(cond ( (= 1 (checkDiag temp_list))
						(print (list 'found 'working 'permutation!))
						(print (list temp_list '==> ' took count 'shuffles))
						(return nil)
					)	
				)
				;; set new_list to be newlist - temp_list
				(setf new_list (remove temp_list new_list))
				(when (eql new_list '())(return))
			)
			)
			)
		)
	)
)
(values)
)
