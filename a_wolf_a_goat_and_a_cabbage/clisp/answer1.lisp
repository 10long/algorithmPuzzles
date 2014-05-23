(defun make-state (man wolf goat cabbage) 
  (list man wolf goat cabbage))

(defun man-side (state)
  (nth 0 state))

(defun wolf-side (state)
  (nth 1 state))

(defun goat-side (state)
  (nth 2 state))

(defun cabbage-side (state)
  (nth 3 state))

(defun man-takes-self (state)
  (safe (make-state (opposite (man-side state))
					(wolf-side state)
					(goat-side state)
					(cabbage-side state))))

(defun man-takes-wolf (state)
  (cond ((equal (man-side state) (wolf-side state))
		 (safe (make-state (opposite (man-side state))
						   (opposite (wolf-side state))
						   (goat-side state)
						   (cabbage-side state))))
		(t nil)))

(defun man-takes-goat (state)
  (cond ((equal (man-side state) (goat-side state))
		 (safe (make-state (opposite (man-side state))
						   (wolf-side state)
						   (opposite (goat-side state))
						   (cabbage-side state))))
		(t nil)))

(defun man-takes-cabbage (state)
  (cond ((equal (man-side state) (cabbage-side state))
		 (safe (make-state (opposite (man-side state))
						   (wolf-side state)
						   (goat-side state)
						   (opposite (cabbage-side state)))))
		(t nil)))


(defun opposite (side)
  (cond ((equal side 'east) 'west)
		((equal side 'west) 'east)))

(defun safe (state)
  (cond ((and (equal (goat-side state) (wolf-side state))
			  (not (equal (man-side state) (wolf-side state))))
		 nil)
		((and (equal (goat-side state) (cabbage-side state))
			  (not (equal (man-side state) (goat-side state))))
		 nil)
		(t state)))

(defun path (state goal &optional (been-list nil))
  (cond ((null state) nil)
		((equal state goal) (reverse (cons state been-list)))
		((not (member state been-list :test #'equal))
		 (or (path (man-takes-self state) goal (cons state been-list))
			 (path (man-takes-wolf state) goal (cons state been-list))
			 (path (man-takes-goat state) goal (cons state been-list))
			 (path (man-takes-cabbage state) goal (cons state been-list)))
		 )))

;;Execution
(defun cross-the-river ()
  (let ((start (make-state 'east 'east 'east 'east))
	(goal (make-state 'west 'west 'west 'west)))
    (path start goal)))
