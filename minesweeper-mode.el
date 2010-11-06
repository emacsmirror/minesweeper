(defvar minesweeper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'minesweeper-choose)
    (define-key map (kbd "x") 'minesweeper-choose)
    (define-key map (kbd "RET") 'minesweeper-choose)
    (define-key map (kbd "m") 'minesweeper-toggle-mark)
    (define-key map (kbd "b") 'backward-char)
    (define-key map (kbd "f") 'forward-char)
    (define-key map (kbd "C-n") 'minesweeper-forward-line)
    (define-key map (kbd "n") 'minesweeper-forward-line)
    (define-key map (kbd "p") (lambda () (interactive) (minesweeper-forward-line -1)))
    (define-key map (kbd "C-p") (lambda () (interactive) (minesweeper-forward-line -1)))
    (define-key map (kbd "c") 'minesweeper-choose-around)
    (define-key map (kbd "s") 'minesweeper-show-neighbors)
    map))

(defun minesweeper () "Minesweeper" "Major mode for playing Minesweeper in Emacs.
\\{minesweeper-mode-map}"
  (interactive)
  (switch-to-buffer "minesweeper")
  (kill-all-local-variables)
  (use-local-map minesweeper-mode-map)
  (setq major-mode 'minesweeper-mode)
  (setq mode-name "Minesweeper")
  ;; (setq font-lock-defaults '(minesweeper-font-faces))
  (toggle-read-only t)
  (minesweeper-begin-game))


(defface minesweeper-blank
  '((t (:foreground "black"))) "face for blank spaces")

(defface minesweeper-marked
  '((t (:foreground "black"))) "face for marked spaces")

(defface minesweeper-0
  '((t (:foreground "Grey"))) "face for zero spaces")

(defface minesweeper-1
  '((t (:foreground "#2020FF"))) "face for 1 spaces")

(defface minesweeper-2
  '((t (:foreground "#00C000"))) "face for 2 spaces")

(defface minesweeper-3
  '((t (:foreground "#6000A0"))) "face for 3 spaces")

(defface minesweeper-4
  '((t (:foreground "#C00000"))) "face for 4 spaces")

(defface minesweeper-5
  '((t (:foreground "#008080"))) "face for 5 spaces")

(defface minesweeper-6
  '((t (:foreground "#FF8000"))) "face for 6 spaces")

(defface minesweeper-7
  '((t (:foreground "#A06000"))) "face for 7 spaces")

(defface minesweeper-8
  '((t (:foreground "#FF0000"))) "face for 8 spaces")

(defface minesweeper-neighbor
  '((t (:background "#C0FFFF"))) "face for the neighbors of point")

(defvar minesweeper-font-faces
  '(("-" . 'minesweeper-blank)
    ("*" . 'minesweeper-marked)
    ("0" . 'minesweeper-0)
    ("1" . 'minesweeper-1)
    ("2" . 'minesweeper-2)
    ("3" . 'minesweeper-3)
    ("4" . 'minesweeper-4)
    ("5" . 'minesweeper-5)
    ("6" . 'minesweeper-6)
    ("7" . 'minesweeper-7)
    ("8" . 'minesweeper-8))
  "Font lock rules for minesweeper.")

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

(defvar minesweeper-marks nil
  "Holds 't in (x, y) iff (x, y) has been marked. A marked square cannot be chosen.")

(defvar minesweeper-blanks-left 0
  "Holds the number of mines left. After 'minesweeper-init has been called, the user will win the game when this becomes zero again.")

(defvar *minesweeper-debug* nil
  "when 't, print debugging information.")

(defvar minesweeper-first-move 't
  "If 't, the next move is the first move. So if a mine is selected, move that mine elsewhere")

(defvar minesweeper-wins 0
  "The number of times the player has won the game this session")

(defvar minesweeper-losses 0
  "The number of times the player has lost the game this session")

(defvar minesweeper-game-started nil
  "The time the current game started.")

(defvar minesweeper-top-overlay
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'minesweeper-neighbor)
    overlay)
  "The overlay to go above point")

(defvar minesweeper-left-overlay
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'minesweeper-neighbor)
    overlay)
  "The overlay to go left of point")

(defvar minesweeper-right-overlay
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'minesweeper-neighbor)
    overlay)
  "The overlay to go right of point")

(defvar minesweeper-bottom-overlay
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'minesweeper-neighbor)
    overlay)
  "The overlay to go below point")

(defvar minesweeper-mark-count
  0
  "The number of mines the user has marked.")

(defvar minesweeper-faces
  (let ((table (make-hash-table :test 'equal)))
    (puthash ?0 minesweeper-0 table)
    (puthash ?1 minesweeper-1 table)
    (puthash ?2 minesweeper-2 table)
    (puthash ?3 minesweeper-3 table)
    (puthash ?4 minesweeper-4 table)
    (puthash ?5 minesweeper-5 table)
    (puthash ?6 minesweeper-6 table)
    (puthash ?7 minesweeper-7 table)
    (puthash ?8 minesweeper-8 table)
    (puthash ?- minesweeper-blank table)
    (puthash ?* minesweeper-marked table)
    table)
  "The hashtable mapping a character to the face it should have.")

(defun minesweeper-begin-game (&optional width height mines)
  (if (y-or-n-p (concat (number-to-string (or width minesweeper-default-width))
			" by "
			(number-to-string (or height minesweeper-default-height))
			" with "
			(number-to-string (or mines minesweeper-default-mines))
			" mines ok? "))
      (minesweeper-init (or width minesweeper-default-width)
			(or height minesweeper-default-height)
			(or mines minesweeper-default-mines))
    (let ((width (minesweeper-get-integer "Minefield width? " (number-to-string (or width minesweeper-default-width))))
	  (height (minesweeper-get-integer "Minefield height? " (number-to-string (or height minesweeper-default-height))))
	  (mines (minesweeper-get-integer "Number of mines? " (number-to-string (or mines minesweeper-default-mines)))))
      (minesweeper-init width height mines)))
  (minesweeper-print-field)
  (message "Good luck!"))

(defun minesweeper-init (&optional width height mines)
  "Begin a game of Minesweeper with a board that's 'width by 'height size containing 'mines mines."
  (setq minesweeper-board-width (or width minesweeper-default-width)
	minesweeper-board-height (or height minesweeper-default-height)
	minesweeper-mines (or mines minesweeper-default-mines)
	minesweeper-field (make-hash-table :test 'equal)
	minesweeper-reveals (make-hash-table :test 'equal)
	minesweeper-marks (make-hash-table :test 'equal)
	minesweeper-blanks-left (- (* minesweeper-board-width
				     minesweeper-board-height)
				  minesweeper-mines)
	minesweeper-first-move 't
	minesweeper-game-started (current-time)
	minesweeper-mark-count 0)
  (minesweeper-fill-field))


(defun minesweeper-fill-field ()
  "Fills 'minesweeper-field with 'minesweeper-mines mines, and builds the neighbor count."
  (minesweeper-for x 0 minesweeper-board-width
       (minesweeper-for y 0 minesweeper-board-height
	    (minesweeper-set-mine x y ?0)
	    (minesweeper-hide x y)
	    (minesweeper-unmark x y)))
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
  "If reveal is true, or if the selected mine has been revealed, returns the value at position (x, y), where the origin is the upper left corner of the minefield. Otherwise, it returns * if the square is marked, - if it is not"
  (minesweeper-debug "called view-mine " (number-to-string x) " " (number-to-string y) " " (if reveal "reveal!" "hide"))
  (cond ((or reveal
	     (minesweeper-is-revealed x y))
	 (gethash (list x y)
		  minesweeper-field))
	((minesweeper-marked x y)
	 ?*)
	('t
	 ?-)))

(defun minesweeper-set-mine (x y val)
  "Inserts val into the mine at (x, y)"
  (puthash (list x y)
	   val
	   minesweeper-field))

(defun minesweeper-reveal (x y)
  "Reveals (x, y)."
  (puthash (list x y)
	   't
	   minesweeper-reveals))

(defun minesweeper-hide (x y)
  "Hides (x, y)."
  (puthash (list x y)
	   nil
	   minesweeper-reveals))

(defun minesweeper-is-revealed (x y)
  "Returns 't if (x, y) is revealed, nil otherwise"
  (gethash (list x y)
	   minesweeper-reveals))

(defun minesweeper-mark (x y)
  "Marks the square (x, y) as having a mine. It can't be selected until it is unmarked"
  (unless (minesweeper-marked x y)
    (puthash (list x y)
	     't
	     minesweeper-marks)
    (setq minesweeper-mark-count (1+ minesweeper-mark-count))))


(defun minesweeper-unmark (x y)
  "Removes the mark from (x, y). It can now be selected."
  (when (minesweeper-marked x y)
    (puthash (list x y)
	     nil
	     minesweeper-marks)
    (setq minesweeper-mark-count (1- minesweeper-mark-count))))

(defun minesweeper-marked (x y)
  "Returns 't if (x, y) is marked as having a mine, nil otherwise"
  (gethash (list x y)
	   minesweeper-marks))

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
    (minesweeper-for newx (1- x) (1+ x)
	 (minesweeper-for newy (1- y) (1+ y)
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
  (minesweeper-debug "Printing out the field")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (minesweeper-for y 0 (1- minesweeper-board-height)
	 (minesweeper-for x 0 (1- minesweeper-board-width)
              (let ((val (minesweeper-view-mine x y reveal))
		    (overlay (make-overlay 0 0)))
		(insert-char val 1)
		(overlay-put overlay 'face (minesweeper-get-face val))
		(move-overlay overlay (point) (1+ (point)))))
	 (newline))
    (unless reveal
      (newline)
      (insert (number-to-string minesweeper-mark-count)
	      " of "
	      (number-to-string minesweeper-mines)
	      " marked.")))
  (minesweeper-debug "Field is printed out"))

(defun minesweeper-pick (x y)
  "Select the square at position (x, y) to reveal."
  (minesweeper-debug "starting pick with args:" (number-to-string x) " " (number-to-string y))
  (unless (or (>= x minesweeper-board-width)
	      (>= y minesweeper-board-height)
	      (minesweeper-is-revealed x y)
	      (minesweeper-marked x y))
    (minesweeper-debug "in pick, valid position chosen")
    (when minesweeper-first-move
      (minesweeper-debug "in pick, first-move is on. Calling view-mine.")
      (when (eq (minesweeper-view-mine x y 't)
	      ?X)
	(minesweeper-debug "On the first move, the user picked a mine. Moving it away")
	(minesweeper-move-mine-away x y))
      (setq minesweeper-first-move nil))
    (minesweeper-debug "in pick, done with first-move check. Getting the value of the square.")
    (let ((val (minesweeper-view-mine x y 't)))
      (minesweeper-debug "view-mine called. The value at " (number-to-string x) ", " (number-to-string y) " is " (make-string 1 val))
      (if (eq val ?X)
	  (progn (minesweeper-lose-game x y)
		 (throw 'game-end nil))
	(let ((to-reveal (list (list x y))))
	  (minesweeper-debug "The user didn't pick an X")
	  (while to-reveal
	    (let* ((cur (pop to-reveal))
		   (cur-x (car cur))
		   (cur-y (cadr cur))
		   (val (minesweeper-view-mine cur-x cur-y 't)))
	      (minesweeper-debug "View-mine says " (number-to-string cur-x) ", " (number-to-string cur-y) " mine = " (make-string 1 val))
	      (unless (minesweeper-is-revealed cur-x cur-y)
		(minesweeper-debug "it's not revealed, so reveal it")
		(minesweeper-reveal cur-x cur-y)
		(if (eq (setq minesweeper-blanks-left (1- minesweeper-blanks-left))
			0)
		    (progn (minesweeper-win-game)
			   (throw 'game-end nil))
		  (when (eq val ?0)
		    (minesweeper-debug "pushing neighbors onto the stack")
		    (mapcar '(lambda (position)
			       (push position
				     to-reveal))
			    (minesweeper-neighbors cur-x cur-y))))))))))))


(defun minesweeper-toggle-mark ()
  "Set the marked status of the current square to the opposite of what it currently is"
  (interactive)
  (minesweeper-refresh-field
   (let ((col (current-column))
	 (row (1- (line-number-at-pos))))
     (unless (minesweeper-is-revealed col row)
       (if (minesweeper-marked col row)
	   (minesweeper-unmark col row)
	 (minesweeper-mark col row))))))

(defun minesweeper-move-mine-away (x y)
  "Moves a mine away from (x, y) to another random position. It updates the values in the minefield to account for neighbor changes."
  (minesweeper-debug "in move-mine-away")
  (minesweeper-insert-mines 1 x y)
  (let ((mine-count 0))
    (minesweeper-debug "in move-mine-away, calling map")
    (mapcar '(lambda (square) (when (eq (minesweeper-view-mine (car square) (cadr square) 't)
				     ?X)
			     (setq mine-count (1+ mine-count))))
	    (minesweeper-neighbors x y))
    (minesweeper-set-mine x y (+ ?0 mine-count)))
  (mapcar '(lambda (square) (minesweeper-++ (car square) (cadr square) -1))
       (minesweeper-neighbors x y))
  (minesweeper-debug "in move-mine-away, calling pick")
  (minesweeper-pick x y))

(defun minesweeper-choose ()
  "This is the function called when the user picks a mine."
  (interactive)
  (minesweeper-debug "starting choose")
  (minesweeper-refresh-field
   (let ((col (current-column))
	 (row (1- (line-number-at-pos))))
     (catch 'game-end (minesweeper-pick col row))))
  (minesweeper-debug "finishing choose"))

(defun minesweeper-choose-around ()
  "This is the function called by the user to pick all non-marked cells around point. It does not include the cell at point."
  (interactive)
  (minesweeper-debug "starting choose-around")
  (minesweeper-refresh-field
   (let ((col (current-column))
	 (row (1- (line-number-at-pos))))
     (catch 'game-end (minesweeper-pick-around col row))))
  (minesweeper-debug "finishing choose-around"))


(defun minesweeper-pick-around (x y)
  "Pick all the squares around (x, y). As a precondition, (x, y) should be zero."
  (minesweeper-debug "called pick-around " (number-to-string x) " " (number-to-string y))
  (mapcar '(lambda (position)
	     (minesweeper-debug "called pick-around-helper " (number-to-string x) " " (number-to-string y))
	     (minesweeper-pick (car position) (cadr position)))
	  (minesweeper-neighbors x y)))

(defun minesweeper-lose-game (x y)
  "Print the lose-game message and prompt for a new one."
  (let ((game-duration (time-subtract (current-time) minesweeper-game-started)))
    (minesweeper-end-game (concat "You lose. This game took "
				  (format-seconds "%H, %M, %S. " (+ (* (car game-duration)
								       (expt 2 16))
								    (cadr game-duration)))
				  "You chose spot ("
				  (number-to-string x)
				  ", "
				  (number-to-string y)
				  ") which was a bomb. You've won "
				  (number-to-string minesweeper-wins)
				  " and lost "
				  (number-to-string (setq minesweeper-losses (1+ minesweeper-losses)))
				  ". Another game? "))))

(defun minesweeper-win-game ()
  "Print the win-game message and prompt for a new one."
  (let ((game-duration (time-subtract (current-time) minesweeper-game-started)))
    (minesweeper-end-game (concat "Congrats! You've won in "
				  (format-seconds "%H, %M, %S. " (+ (* (car game-duration)
								       (expt 2 16))
								    (cadr game-duration)))
				  "You've won "
				  (number-to-string (setq minesweeper-wins (1+ minesweeper-wins)))
				  " and lost "
				  (number-to-string minesweeper-losses)
				  ". Another game? "))))

(defun minesweeper-end-game (message)
  "ends the game, prompting for a new game with message"
  (minesweeper-print-field 't)
  (when (y-or-n-p message)
      (minesweeper-begin-game minesweeper-board-width minesweeper-board-height minesweeper-mines)))

(defmacro minesweeper-for (var init end &rest body)
  "Helper function. executes 'body repeatedly, with 'var assigned values starting at 'init, and ending at 'end, increasing by one each iteration."
  `(let ((,var ,init)
	 (end-val ,end))
     (while (<= ,var end-val)
       ,@body
       (setq ,var (1+ ,var)))))

(defmacro minesweeper-debug (&rest body)
  "If *minesweeper-debug* is 't, log ,@body as a string to the buffer named 'debug'"
  `(when *minesweeper-debug*
     (print (concat ,@body)
	    (get-buffer-create "debug"))))

(defmacro minesweeper-refresh-field (&rest body)
  "executes the body code, and prints out the new minefield, putting point back where it was when this macro was called. Binds 'col and 'row to appropriate values."
  (let ((col (make-symbol "col"))
	(row (make-symbol "row")))
    `(let ((,col (current-column)) ;; make use gensyms
	   (,row (1- (line-number-at-pos))))
       ,@body
       (minesweeper-print-field)
       (goto-char (point-min))
       (forward-char ,col)
       (minesweeper-forward-line ,row))))

(defun minesweeper-get-integer (&optional message default)
  "Reads one nonzero integer from the minibuffer."
  (let ((val (string-to-number (read-string (or message "Input an integer:")
					    (or default "0")))))
    (while (eq val 0)
      (setq val (string-to-number (read-string (concat (or message "Input an integer")
						       ". Please, a nonzero integer. Try again:")
					       (or default "0")))))
    val))

(defun minesweeper-forward-line (&optional lines)
  "Moves one line forward, keeping point at the same column."
  (interactive)
  (let ((col (current-column)))
    (forward-line (or lines 1))
    (forward-char col)))

(defun minesweeper-show-neighbors ()
  (interactive)
  (let ((col (current-column))
	(row (1- (line-number-at-pos)))
	(point (point)))
    (unless (or (>= col minesweeper-board-width)
		(>= row minesweeper-board-height))
      (if (eq row 0);; "top" overlay
	  (move-overlay minesweeper-top-overlay 0 0 (get-buffer "minesweeper"))
	(let ((center (- point minesweeper-board-width 1)))
	  (move-overlay minesweeper-top-overlay
			(- center (min col 1))
			(+ center 1 (if (>= col (1- minesweeper-board-width)) 0 1))
			(get-buffer "minesweeper"))))

      (if (eq col 0);; "left" overlay
	  (move-overlay minesweeper-left-overlay 0 0 (get-buffer "minesweeper"))
	(move-overlay minesweeper-left-overlay (1- point) point (get-buffer "minesweeper")))

      (if (>= col (1- minesweeper-board-width)) ;; "right" overlay
	  (move-overlay minesweeper-right-overlay 0 0 (get-buffer "minesweeper"))
	(move-overlay minesweeper-right-overlay (1+ point) (+ point 2) (get-buffer "minesweeper")))

      (if (>= row (1- minesweeper-board-height)) ;; "bottom" overlay
	  (move-overlay minesweeper-bottom-overlay 0 0 (get-buffer "minesweeper"))
	(let ((center (+ point minesweeper-board-width 1)))
	  (move-overlay minesweeper-bottom-overlay
			(- center (if (eq col 0) 0 1))
			(+ center 1 (if (>= col (1- minesweeper-board-width)) 0 1))
			(get-buffer "minesweeper")))))))

(defun minesweeper-get-face (val)
  (gethash val minesweeper-faces))