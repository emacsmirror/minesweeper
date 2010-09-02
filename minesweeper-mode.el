(defvar minesweeper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'minesweeper-choose)
    (define-key map (kbd "x") 'minesweeper-choose)
    (define-key map (kbd "RET") 'minesweeper-choose)
    map))

(define-derived-mode minesweeper-mode nil "Minesweeper" "Major mode for playing Minesweeper in Emacs.
\\{minesweeper-mode-map}"
  (kill-all-local-variables)
  (use-local-map minesweeper-mode-map)
  (setq major-mode 'minesweeper-mode)
  (setq mode-name "Minesweeper")
  (toggle-read-only t)
  (minesweeper-init)
  (minesweeper-print-field))

(defvar minesweeper-board-width 0
  "The number of columns on the Minesweeper field.")

(defvar minesweeper-default-width 10
  "The default board width")

(defvar minesweeper-board-height 0
  "The number of rows on the Minesweeper field.")

(defvar minesweeper-default-height 10
  "The default board height.")

(defvar minesweeper-mines 0
  "The number of mines on the Minesweeper field.")

(defvar minesweeper-default-mines 10
  "The default number of mines")

(defvar minesweeper-field nil
  "The minefield itself. If a mine is in the square, ?X is stored. Otherwise, the number of mines in neighboring squares is stored.")

(defvar minesweeper-reveals nil
  "Holds 't in (x, y) if (x, y) has been revealed")

(defvar minesweeper-blanks-left 0
  "Holds the number of mines left. After 'minesweeper-init has been called, the user will win the game when this becomes zero again.")

(defvar *debug* nil
  "when 't, print debugging information.")

(defvar minesweeper-first-move 't
  "If 't, the next move is the first move. So if a mine is selected, move that mine elsewhere")


(defun minesweeper-init (&optional width height mines)
  "Begin a game of Minesweeper with a board that's 'width by 'height size containing 'mines mines."
  (setq minesweeper-board-width (or width minesweeper-default-width)
	minesweeper-board-height (or height minesweeper-default-height)
	minesweeper-mines (or mines minesweeper-default-mines)
	minesweeper-field (make-hash-table :test 'equal)
	minesweeper-reveals (make-hash-table :test 'equal)
	minesweeper-blanks-left (- (* minesweeper-board-width
				     minesweeper-board-height)
				  minesweeper-mines)
	minesweeper-first-move 't)
  (minesweeper-fill-field))


(defun minesweeper-fill-field ()
  "Fills 'minesweeper-field with 'minesweeper-mines mines, and builds the neighbor count."
  (for x 0 minesweeper-board-width
       (for y 0 minesweeper-board-height
	    (minesweeper-set-mine x y ?0)
	    (minesweeper-set-revealed x y nil)))
  (minesweeper-insert-mines minesweeper-mines))

(defun minesweeper-insert-mines (count &optional protect-x protect-y)
  (while (> count 0)
    (let ((x (random minesweeper-board-width))
	  (y (random minesweeper-board-height)))
      (unless (or (eq (minesweeper-view-mine x y 't)
		      ?X)
		  (and (eq x protect-x)
		       (eq y protect-y)))
	(minesweeper-set-mine x y ?X)
	(minesweeper-inform-around x y)
	(setq count (1- count))))))


(defun minesweeper-view-mine (x y &optional reveal)
  "If reveal is true, or if the selected mine has been revealed, returns the value at position (x, y), where the origin is the upper left corner of the minefield. Otherwise, it returns '_'"
  (debug "called view-mine " (char-to-string (+ x ?0)) " " (char-to-string (+ y ?0)) " " (if reveal "reveal!" "hide"))
  (if (or reveal
	  (minesweeper-is-revealed x y))
      (gethash (list x y)
	       minesweeper-field)
    ?_))

(defun minesweeper-set-mine (x y val)
  "Inserts val into the mine at (x, y)"
  (puthash (list x y)
	   val
	   minesweeper-field))

(defun minesweeper-set-revealed (x y val)
  "Sets (x, y) in the filter to 'val"
  (puthash (list x y)
	   val
	   minesweeper-reveals))

(defun minesweeper-is-revealed (x y)
  (gethash (list x y)
	   minesweeper-reveals))

(defun minesweeper-inform-around (x y &optional amount)
  "takes in a square, and increases the values of all its empty neighbors by 'amount"
  (mapcar (lambda (position)
	    (minesweeper-++ (car position) (cadr position) (or amount 1)))
	  (minesweeper-neighbors x y)))

(defun minesweeper-++ (x y &optional amount)
  "Increments the value at square (x, y), unless the square is a bomb"
  (let ((val (minesweeper-view-mine x y 't)))
    (when (and (<= ?0 val)
	       (< val ?9))
      (minesweeper-set-mine x
			    y
			    (+ val
			       (or amount 1))))))

(defun minesweeper-neighbors (x y)
  "Returns a list of the neighbors of (x, y)."
  (let ((neighbors nil))
    (for newx (1- x) (1+ x)
	 (for newy (1- y) (1+ y)
	      (unless (or (and (eq newx x)
			       (eq newy y))
			  (< newx 0)
			  (< newy 0)
			  (>= newx minesweeper-board-width)
			  (>= newy minesweeper-board-height))
		(push (list newx newy)
		      neighbors))))
    neighbors))

(defun minesweeper-print-field (&optional reveal)
  "Print out the minefield."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (for y 0 (1- minesweeper-board-height)
	 (for x 0 (1- minesweeper-board-width)
	      (insert-char (minesweeper-view-mine x y reveal) 1))
	 (newline))))


(defun minesweeper-pick (x y &optional suppress-field)
  "Select the square at position (x, y) to reveal. A user-facing function."
    (debug "pick " (char-to-string (+ ?0 x)) " " (char-to-string (+ ?0 y)))
  (unless (or (>= x minesweeper-board-width)
	      (>= y minesweeper-board-height)
	      (minesweeper-is-revealed x y))
    (when minesweeper-first-move
      (when (eq (minesweeper-view-mine x y 't)
	      ?X)
	(minesweeper-move-mine-away x y))
      (setq minesweeper-first-move nil))
    (debug "in pick, done with first-move check")
    (let ((val (minesweeper-view-mine x y 't)))
      (debug "called view-mine")
      (if (eq val ?X)
	  (minesweeper-lose-game x y)
	(progn
	  (minesweeper-set-revealed x y 't)
	  (when (eq val ?0)
	    (let ((max-lisp-eval-depth (* 25 minesweeper-board-width minesweeper-board-height)))
	      (minesweeper-pick-around x y)))
	  (if (eq (setq minesweeper-blanks-left
			(1- minesweeper-blanks-left))
		  0)
	      (minesweeper-win-game)
	    (unless suppress-field
	      (minesweeper-print-field))))))))




(defun minesweeper-move-mine-away (x y)
  (minesweeper-insert-mines 1 x y)
  (let ((mine-count 0))
    	  (map '(lambda (square) (when (eq (minesweeper-view-mine (car square) (cadr square) 't)
					   ?X)
				   (++ mine-count)))
	       (minesweeper-neighbors x y))
	  (minesweeper-set-mine x y mine-count))
  (map '(lambda (square) (minesweeper-++ (car square) (cadr square) -1))
       (minesweeper-neighbors x y))
  (minesweeper-pick x y))

(defun minesweeper-choose ()
  "This is the function called when the user picks a mine."
  (interactive)
  (debug "minesweeper-choose")
  (let ((col (current-column))
	(row (minesweeper-current-line)))
    (minesweeper-pick col row)
    (goto-char (point-min))
    (forward-char col)
    (next-line row)))

(defun minesweeper-pick-around (x y)
  "Pick all the squares around (x, y). As a precondition, (x, y) should be zero."
  (debug "called pick-around " (char-to-string (+ ?0 x)) " " (char-to-string (+ ?0 y)) (newline))
  (mapcar '(lambda (position)
	     (debug "called pick-around-helper " (char-to-string (+ ?0 x)) " " (char-to-string (+ ?0 y)))
	     (minesweeper-pick (car position) (cadr position) 't))
	  (minesweeper-neighbors x y)))

(defun minesweeper-lose-game (x y)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (minesweeper-print-field 't)
    (newline 2)
    (message (concat "You lose. You chose spot (" (char-to-string (+ x ?0)) ", " (char-to-string (+ y ?0)) ") which was a bomb."))))




(defun minesweeper-win-game ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (minesweeper-print-field 't)
    (newline 2)
    (message "You win! Congrats!")))


(defmacro for (var init end &rest body)
  "helper function. executes 'body repeatedly, with 'var assigned values starting at 'init, and ending at 'end, increasing by one each iteration."
  `(let ((,var ,init)
	 (end-val ,end))
     (while (<= ,var end-val)
       ,@body
       (setq ,var (1+ ,var)))))

(defmacro debug (&rest body)
  `(when *debug*
     (print (concat ,@body)
	    (get-buffer-create "debug"))))

(defun minesweeper-current-line ()
  (save-excursion
    (let ((line 0))
      (move-beginning-of-line nil)
      (while (not (bobp))
	     (setq line (1+ line))
	     (previous-line))
      line)))




