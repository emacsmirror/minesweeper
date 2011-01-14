(defvar minesweeper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'minesweeper-choose)
    (define-key map (kbd "x") 'minesweeper-choose)
    (define-key map (kbd "RET") 'minesweeper-choose)
    (define-key map [mouse-1] 'minesweeper-choose)
    (define-key map (kbd "m") 'minesweeper-toggle-mark)
    (define-key map [mouse-3] 'minesweeper-toggle-mark-mouse)
    (define-key map (kbd "b") 'backward-char)
    (define-key map (kbd "f") 'forward-char)
    (define-key map (kbd "C-n") 'next-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "C-p") 'previous-line)
    (define-key map (kbd "c") 'minesweeper-choose-around)
    (define-key map [mouse-2] 'minesweeper-choose-around-mouse)
    (define-key map (kbd "s") 'minesweeper-toggle-show-neighbors)
    map))

(defun minesweeper () "Major mode for playing Minesweeper in Emacs. There's a field of squares; each square may hold a mine. If a square doesn't have a mine, you'll see how many mines are in the eight neighboring squares. You may mark squares, which protects them from accidentally being selected.

Your goal is to reveal all the empty squares.

\\{minesweeper-mode-map}"
  (interactive)
  (switch-to-buffer "minesweeper")
  (kill-all-local-variables)
  (use-local-map minesweeper-mode-map)
  (setq major-mode 'minesweeper-mode)
  (setq mode-name "Minesweeper")
  (toggle-read-only t)
  (when *minesweeper-idle-timer*
    (cancel-timer *minesweeper-idle-timer*))
  (setq *minesweeper-idle-timer* (run-with-idle-timer *minesweeper-idle-delay*
						      t
						      'minesweeper-show-neighbors))
  (minesweeper-begin-game))

(defun minesweeper-mode () "Major mode for playing Minesweeper. To learn how to play minesweeper, see the documentation for 'minesweeper'." nil)

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

(defvar *minesweeper-board-width* nil
  "The number of columns on the Minesweeper field.")

(defvar *minesweeper-default-width* 10
  "The default board width")

(defvar *minesweeper-board-height* nil
  "The number of rows on the Minesweeper field.")

(defvar *minesweeper-default-height* 10
  "The default board height.")

(defvar *minesweeper-mines* nil
  "The number of mines on the Minesweeper field.")

(defvar *minesweeper-default-mines* 10
  "The default number of mines")

(defvar *minesweeper-field* nil
  "The minefield itself. If a mine is in the square, ?X is stored. Otherwise, the number of mines in neighboring squares is stored.")

(defvar *minesweeper-reveals* nil
  "Holds 't in (x, y) if (x, y) has been revealed")

(defvar *minesweeper-marks* nil
  "Holds 't in (x, y) iff (x, y) has been marked. A marked square cannot be chosen.")

(defvar *minesweeper-blanks-left* 0
  "Holds the number of mines left. After 'minesweeper-init has been called, the user will win the game when this becomes zero again.")

(defvar *minesweeper-debug* nil
  "when 't, print debugging information.")

(defvar *minesweeper-first-move* 't
  "If 't, the next move is the first move. So if a mine is selected, move that mine elsewhere")

(defvar *minesweeper-wins* 0
  "The number of times the player has won the game this session")

(defvar *minesweeper-losses* 0
  "The number of times the player has lost the game this session")

(defvar *minesweeper-game-epoch* nil
  "The time the current game started.")

(defvar *minesweeper-min-free-squares* 1
  "The minimum number of squares which must be free.")

(defvar *minesweeper-top-overlay*
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'minesweeper-neighbor)
    overlay)
  "The overlay to go above point")

(defvar *minesweeper-left-overlay*
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'minesweeper-neighbor)
    overlay)
  "The overlay to go left of point")

(defvar *minesweeper-right-overlay*
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'minesweeper-neighbor)
    overlay)
  "The overlay to go right of point")

(defvar *minesweeper-bottom-overlay*
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'minesweeper-neighbor)
    overlay)
  "The overlay to go below point")

(defvar *minesweeper-mark-count*
  0
  "The number of mines the user has marked.")

(defvar *minesweeper-faces*
  (let ((table (make-hash-table :test 'equal)))
    (puthash ?0 'minesweeper-0 table)
    (puthash ?1 'minesweeper-1 table)
    (puthash ?2 'minesweeper-2 table)
    (puthash ?3 'minesweeper-3 table)
    (puthash ?4 'minesweeper-4 table)
    (puthash ?5 'minesweeper-5 table)
    (puthash ?6 'minesweeper-6 table)
    (puthash ?7 'minesweeper-7 table)
    (puthash ?8 'minesweeper-8 table)
    (puthash ?- 'minesweeper-blank table)
    (puthash ?* 'minesweeper-marked table)
    table)
  "The hashtable mapping a character to the face it should have.")

(defvar *minesweeper-idle-timer* nil
  "The timer used to highlight neighbors")

(defvar *minesweeper-idle-delay* 0.0625
  "The time Emacs must be idle before highlighting the neigbors of point.")

(defvar *minesweeper-game-over* nil
  "t if the user has selected a mine or selected all the empty squares, nil otherwise.")

(defun minesweeper-begin-game (&optional width height mines)
  "Prompt the user for the minefield size and number of mines, then initialize the game."
  (minesweeper-debug "beginning the game")
  (if (y-or-n-p (concat (number-to-string (or width *minesweeper-board-width* *minesweeper-default-width*))
			" by "
			(number-to-string (or height *minesweeper-board-height* *minesweeper-default-height*))
			" with "
			(number-to-string (or mines *minesweeper-mines* *minesweeper-default-mines*))
			" mines ok? "))
      (minesweeper-init (or width *minesweeper-board-width* *minesweeper-default-width*)
			(or height *minesweeper-board-height* *minesweeper-default-height*)
			(or mines *minesweeper-mines* *minesweeper-default-mines*))
    (let ((width (minesweeper-get-integer "Minefield width? " (number-to-string (or width *minesweeper-board-width* *minesweeper-default-width*))))
	  (height (minesweeper-get-integer "Minefield height? " (number-to-string (or height *minesweeper-board-height* *minesweeper-default-height*))))
	  (mines (minesweeper-get-integer "Number of mines? " (number-to-string (or mines *minesweeper-mines* *minesweeper-default-mines*)))))
      (minesweeper-init width height mines)))
  (minesweeper-print-field)
  (goto-char (+ (* (truncate (/ (1- *minesweeper-board-height*)
				2))
		   (1+ *minesweeper-board-width*))
		(ceiling (/ (float *minesweeper-board-width*) 2))))
  (message "Good luck!"))

(defun minesweeper-init (&optional width height mines)
  "Begin a game of Minesweeper with a board that's 'width by 'height size containing 'mines mines."
  (minesweeper-debug "initializing the game")
  (setq *minesweeper-board-width* (or width *minesweeper-default-width*)
	*minesweeper-board-height* (or height *minesweeper-default-height*)
	*minesweeper-mines* (or mines *minesweeper-default-mines*)
	*minesweeper-field* (make-hash-table :test 'equal :size (* *minesweeper-board-width*
								   *minesweeper-board-height*))
	*minesweeper-reveals* (make-hash-table :test 'equal :size (* *minesweeper-board-width*
								     *minesweeper-board-height*))
	*minesweeper-marks* (make-hash-table :test 'equal :size (* *minesweeper-board-width*
								   *minesweeper-board-height*))
	*minesweeper-blanks-left* (- (* *minesweeper-board-width*
					*minesweeper-board-height*)
				     *minesweeper-mines*)
	*minesweeper-first-move* 't
	*minesweeper-game-epoch* (current-time)
	*minesweeper-mark-count* 0
	*minesweeper-game-over* nil)
  (minesweeper-debug "most global vars set -- checking for overpopulation of mines.")
  (while (< *minesweeper-blanks-left* *minesweeper-min-free-squares*)
    (setq *minesweeper-mines* (minesweeper-get-integer (format "Too many mines. You can have at most %d mines. Number of mines?" (- (* *minesweeper-board-width*
																       *minesweeper-board-height*)
																    *minesweeper-min-free-squares*))
						       *minesweeper-default-mines*)
	  *minesweeper-blanks-left* (- (* *minesweeper-board-width*
					  *minesweeper-board-height*)
				       *minesweeper-mines*))))


(defun minesweeper-fill-field (protect-x protect-y)
  "Fills '*minesweeper-field* with '*minesweeper-mines* mines, and builds the neighbor count. It will not place any mines in the square (protect-x, protect-y)."
  (minesweeper-debug "filling the field")
  (minesweeper-for x 0 (1- *minesweeper-board-width*)
		   (minesweeper-debug "inside outer loop -- x is " (number-to-string x))
		   (minesweeper-for y 0 (1- *minesweeper-board-height*)
				    (minesweeper-debug "inside inner loop -- setting up mine " (number-to-string x) " " (number-to-string y))
				    (minesweeper-set-mine x y ?0)
				    (minesweeper-hide x y)
				    (minesweeper-unmark x y)))
  (minesweeper-debug "done setting zeros; now inserting mines")
  (minesweeper-insert-mines *minesweeper-mines* protect-x protect-y))

(defun minesweeper-insert-mines (count protect-x protect-y)
  "insert 'count mines into the minefield, and build up the neighbor count. There can't be a mine at the square (protect-x, protect-y)"
  (minesweeper-debug "inserting " (number-to-string count) " mines")
  (let* ((square-count (1- (* *minesweeper-board-width* *minesweeper-board-height*)))
	 (mines (make-vector square-count (list 0 0)))
	 (pos 0))
    (minesweeper-for x 0 (1- *minesweeper-board-width*)
		     (minesweeper-for y 0 (1- *minesweeper-board-height*)
				      (unless (and (eq x protect-x)
						   (eq y protect-y))
					(minesweeper-debug "setting " (number-to-string x) "\t" (number-to-string y))
					(aset mines pos (list x y))
					(setq pos (1+ pos)))))
    (dotimes (i count)
      (let* ((rand (random (- square-count i)))
	     (ele (aref mines rand)))
	(minesweeper-debug "picked a random mine at position " (number-to-string rand) ". The mine is " (number-to-string (car ele)) "\t" (number-to-string (cadr ele)) ". We've picked " (number-to-string i)" mines so far.")
	(aset mines rand (aref mines (- square-count i 1)))
	(minesweeper-set-mine (car ele) (cadr ele) ?X)
	(minesweeper-inform-around (car ele) (cadr ele))))))

(defun minesweeper-view-mine (x y &optional reveal)
  "If reveal is true, or if the selected mine has been revealed, returns the value at position (x, y), where the origin is the upper left corner of the minefield. Otherwise, it returns * if the square is marked, - if it is not"
  (minesweeper-debug "called view-mine " (number-to-string x) " " (number-to-string y) " " (if reveal "reveal!" "hide"))
  (cond ((or reveal
	     (minesweeper-is-revealed x y))
	 (gethash (list x y)
		  *minesweeper-field*))
	((minesweeper-marked x y)
	 ?*)
	('t
	 ?-)))

(defun minesweeper-set-mine (x y val)
  "Inserts val into the mine at (x, y)"
  (puthash (list x y)
	   val
	   *minesweeper-field*))

(defun minesweeper-reveal (x y)
  "Reveals (x, y)."
  (puthash (list x y)
	   't
	   *minesweeper-reveals*))

(defun minesweeper-hide (x y)
  "Hides (x, y)."
  (puthash (list x y)
	   nil
	   *minesweeper-reveals*))

(defun minesweeper-is-revealed (x y)
  "Returns 't if (x, y) is revealed, nil otherwise"
  (gethash (list x y)
	   *minesweeper-reveals*))

(defun minesweeper-mark (x y)
  "Marks the square (x, y) as having a mine. It can't be selected until it is unmarked"
  (minesweeper-debug "marking square " (number-to-string x) "\t" (number-to-string y))
  (unless (minesweeper-marked x y)
    (puthash (list x y)
	     't
	     *minesweeper-marks*)
    (setq *minesweeper-mark-count* (1+ *minesweeper-mark-count*))))

(defun minesweeper-unmark (x y)
  "Removes the mark from (x, y). It can now be selected."
  (when (minesweeper-marked x y)
    (puthash (list x y)
	     nil
	     *minesweeper-marks*)
    (setq *minesweeper-mark-count* (1- *minesweeper-mark-count*))))

(defun minesweeper-invert-mark (x y)
  "If (x, y) is marked, unmark it. Otherwise, mark it."
  (when (and (minesweeper-in-bounds x y)
	     (not (minesweeper-is-revealed x y)))
    (if (minesweeper-marked x y)
	(minesweeper-unmark x y)
      (minesweeper-mark x y))))

(defun minesweeper-marked (x y)
  "Returns 't if (x, y) is marked as having a mine, nil otherwise"
  (gethash (list x y)
	   *minesweeper-marks*))

(defun minesweeper-inform-around (x y &optional amount)
  "takes in a square, and increases the values of all its empty neighbors by 'amount"
  (mapc (lambda (position)
	  (minesweeper-++ (car position) (cdr position) (or amount 1)))
	(minesweeper-neighbors x y)))

(defun minesweeper-++ (x y &optional amount)
  "Increments the value at square (x, y), unless the square is a bomb"
  (let ((val (minesweeper-view-mine x y 't)))
    (when (and (<= ?0 val)
	       (<= val ?8))
      (minesweeper-set-mine x
			    y
			    (+ val
			       (or amount 1))))))

(defun minesweeper-neighbors (x y)
  "Returns a list of the neighbors of (x, y)."
  (let ((neighbors nil))
    (minesweeper-for newx
		     (max (1- x) 0)
		     (min (1+ x) (1- *minesweeper-board-width*))
		     (minesweeper-for newy
				      (max (1- y) 0)
				      (min (1+ y) (1- *minesweeper-board-height*))
				      (when (not (and (eq newx x)
						      (eq newy y)))
					(push (cons newx newy)
					      neighbors))))
    neighbors))

(defun minesweeper-print-field (&optional reveal)
  "Print out the minefield."
  (minesweeper-debug "Printing out the field")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (minesweeper-for y 0 (1- *minesweeper-board-height*)
		     (minesweeper-for x 0 (1- *minesweeper-board-width*)
				      (minesweeper-insert-value (minesweeper-view-mine x y reveal)))
		     (newline))
    (unless reveal
      (newline)
      (insert (number-to-string *minesweeper-mark-count*)
	      " of "
	      (number-to-string *minesweeper-mines*)
	      " marked.")))
  (minesweeper-debug "Field is printed out"))

(defun minesweeper-refresh-field ()
  "Prints out the new minefield, putting point back where it was when this function was called."
  (let ((col (current-column))
	(row (1- (line-number-at-pos))))
    (minesweeper-print-field)
    (goto-char (point-min))
    (forward-char col)
    (next-line row)))

(defun minesweeper-refresh-square (col row)
  "Refreshes the printed value of (col, row)"
  (minesweeper-debug "starting refresh-square. (col, row) is (" (number-to-string col) ",\t" (number-to-string row) ")")
  (let ((val (minesweeper-view-mine col row)))
    (goto-line (1+ row))
    (forward-char col)
    (let ((inhibit-read-only t))
      (delete-char 1)
      (minesweeper-insert-value (minesweeper-view-mine col row)))
    (forward-char -1)))

(defun minesweeper-insert-value (val)
  "Outputs val, properly colored, at point."
  (insert-char val 1)
  (add-text-properties (point)
		       (1- (point))
		       (list 'face
			     (minesweeper-get-face val))))

(defun minesweeper-pick (x y)
  "Reveals the square at position (x, y). If the square is zero, "
  (minesweeper-debug "starting pick with args:" (number-to-string x) " " (number-to-string y))
  (unless (or (not (minesweeper-in-bounds x y))
	      (minesweeper-is-revealed x y)
	      (minesweeper-marked x y))
    (minesweeper-debug "in pick, valid position chosen")
    (when *minesweeper-first-move*
      (minesweeper-debug "in pick, first-move is on. Calling view-mine.")
      (minesweeper-fill-field x y)
      (setq *minesweeper-first-move* nil))
    (minesweeper-debug "in pick, done with first-move check. Getting the value of the square.")
    (let ((val (minesweeper-view-mine x y 't)))
      (minesweeper-debug "view-mine called. The value at " (number-to-string x) ", " (number-to-string y) " is " (make-string 1 val))
      (if (eq val ?X)
	  (progn (minesweeper-lose-game x y)
		 (throw 'game-end nil))
	(let ((to-reveal (list (cons x y))))
	  (minesweeper-debug "The user didn't pick an X")
	  (while to-reveal
	    (let* ((cur (pop to-reveal))
		   (cur-x (car cur))
		   (cur-y (cdr cur)))
	      (minesweeper-debug "View-mine says " (number-to-string cur-x) ", " (number-to-string cur-y) " mine = " (make-string 1 (minesweeper-view-mine cur-x cur-y 't)))
	      (unless (or (minesweeper-is-revealed cur-x cur-y)
			  (minesweeper-marked cur-x cur-y))
		(minesweeper-debug "it's not revealed, so reveal it")
		(minesweeper-reveal cur-x cur-y)
		(if (eq (setq *minesweeper-blanks-left* (1- *minesweeper-blanks-left*))
			0)
		    (progn (minesweeper-win-game)
			   (throw 'game-end nil))
		  (when (eq (minesweeper-view-mine cur-x cur-y 't)
			    ?0)
		    (minesweeper-debug "pushing neighbors onto the stack")
		    (mapc '(lambda (position)
			     (push position
				   to-reveal))
			  (minesweeper-neighbors cur-x cur-y))))))))))))


(defun minesweeper-toggle-mark ()
  "Set the marked status of the current square to the opposite of what it currently is"
  (interactive)
  (unless *minesweeper-game-over*
    (let ((col (current-column))
	  (row (1- (line-number-at-pos))))
      (minesweeper-invert-mark col row)
      (minesweeper-refresh-field))))

(defun minesweeper-toggle-mark-mouse (click)
  "Set the marked status of the clicked-on square to the opposite of what it currently is."
  (interactive "e")
  (unless *minesweeper-game-over*
    (let* ((window (elt (cadr click) 0))
	   (pos (elt (cadr click) 6))
	   (col (car pos))
	   (row (cdr pos)))
      (minesweeper-invert-mark col row)
      (select-window window)
      (minesweeper-refresh-field))))


(defun minesweeper-choose ()
  "This is the function called when the user picks a mine."
  (interactive)
  (minesweeper-debug "starting choose")
  (unless *minesweeper-game-over*
    (let ((col (current-column))
	  (row (1- (line-number-at-pos))))
      (catch 'game-end (minesweeper-pick col row)
	     (if (eq (minesweeper-view-mine col row) ?0)
		 (minesweeper-refresh-field)
	       (minesweeper-refresh-square col row))))
    (minesweeper-debug "finishing choose")))

(defun minesweeper-choose-around ()
  "Pick all non-marked cells around point. It does not include the cell at point."
  (interactive)
  (minesweeper-debug "starting choose-around")
  (unless *minesweeper-game-over*
    (let ((col (current-column))
	  (row (1- (line-number-at-pos))))
      (catch 'game-end (minesweeper-pick-around col row)
	     (minesweeper-refresh-field)))
    (minesweeper-debug "finishing choose-round")))

(defun minesweeper-choose-around-mouse (click)
  "Choose all the non-marked cells around the one clicked on, not including the one clicked on."
  (interactive "e")
  (minesweeper-debug "beginning choose-around-mouse")
  (unless *minesweeper-game-over*
    (let ((window (elt (cadr click) 0))
	  (pos (elt (cadr click) 6)))
      (catch 'game-end (minesweeper-pick-around (car pos) (cdr pos))
	     (select-window window)
	     (minesweeper-refresh-field)))
    (minesweeper-debug "ending choose-around-mouse")))

(defun minesweeper-pick-around (x y)
  "Pick all the squares around (x, y). As a precondition, (x, y) should be zero."
  (minesweeper-debug "called pick-around " (number-to-string x) " " (number-to-string y))
  (when (minesweeper-in-bounds x y)
    (mapc '(lambda (position)
	     (minesweeper-debug "called pick-around-helper " (number-to-string x) " " (number-to-string y))
	     (minesweeper-pick (car position) (cdr position)))
	  (minesweeper-neighbors x y))))

(defun minesweeper-lose-game (x y)
  "Print the lose-game message and prompt for a new one."
  (setq *minesweeper-losses* (1+ *minesweeper-losses*))
  (minesweeper-end-game (concat "You lose. This game took "
				(minesweeper-game-duration-message)
				"You chose spot ("
				(number-to-string x)
				", "
				(number-to-string y)
				") which was a bomb. "
				(minesweeper-record-message)
				"Another game? ")))

(defun minesweeper-win-game ()
  "Print the win-game message and prompt for a new one."
  (setq *minesweeper-wins* (1+ *minesweeper-wins*))
  (minesweeper-end-game (concat "Congrats! You've won in "
				(minesweeper-game-duration-message)
				(minesweeper-record-message)
				"Another game? ")))

(defun minesweeper-end-game (message)
  "ends the game, prompting for a new game with message"
  (setq *minesweeper-game-over* t)
  (minesweeper-print-field 't)
  (when (y-or-n-p message)
    (minesweeper-begin-game *minesweeper-board-width* *minesweeper-board-height* *minesweeper-mines*)))

(defun minesweeper-game-duration-message ()
  "Returns the duration the current game has taken as a human-readable string."
  (let ((game-duration (time-subtract (current-time) *minesweeper-game-epoch*)))
    (format-seconds "%H, %M, %S. " (+ (* (car game-duration)
					 (expt 2 16))
				      (cadr game-duration)))))

(defun minesweeper-record-message ()
  "Returns the number of wins and losses formatted as a human-readable string."
  (concat "You've won "
	  (number-to-string *minesweeper-wins*)
	  " games and lost "
	  (number-to-string *minesweeper-losses*)
	  ". "))

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

(defun minesweeper-get-integer (&optional message default)
  "Reads one nonzero integer from the minibuffer."
  (setq default (cond ((not default)
		       "0")
		      ((integerp default)
		       (number-to-string default))
		      ((stringp default)
		       default)
		      (t "0")))
  (let ((val (string-to-number (read-string (concat (or message "Input an integer")
						    " (default "
						    default
						    "):")
					    nil nil default))))
    (while (eq val 0)
      (setq val (string-to-number (read-string (concat (or message "Input an integer")
						       ". Please, a nonzero integer. Try again. (default "
						       default
						       "):")
					       nil nil default))))
    val))

(defun minesweeper-show-neighbors ()
  "If point is within the minefield, highlight as many of the eight squares around point that are in the minefield."
  (minesweeper-reset-neighbor-overlays)
  (when (equal "minesweeper"
	       (buffer-name (current-buffer)))
    (let ((col (current-column))
	  (row (1- (line-number-at-pos)))
	  (point (point)))
      (when (minesweeper-in-bounds col row)
	(when (> row 0);; "top" overlay
	  (let ((center (- point *minesweeper-board-width* 1)))
	    (move-overlay *minesweeper-top-overlay*
			  (- center (min col 1))
			  (+ center 1 (if (>= col (1- *minesweeper-board-width*)) 0 1))
			  (get-buffer "minesweeper"))))
	(when (> col 0);; "left" overlay
	  (move-overlay *minesweeper-left-overlay* (1- point) point (get-buffer "minesweeper")))
	(when (< col (1- *minesweeper-board-width*)) ;; "right" overlay
	  (move-overlay *minesweeper-right-overlay* (1+ point) (+ point 2) (get-buffer "minesweeper")))
	(when (< row (1- *minesweeper-board-height*)) ;; "bottom" overlay
	  (let ((center (+ point *minesweeper-board-width* 1)))
	    (move-overlay *minesweeper-bottom-overlay*
			  (- center (if (eq col 0) 0 1))
			  (+ center 1 (if (>= col (1- *minesweeper-board-width*)) 0 1))
			  (get-buffer "minesweeper"))))))))

(defun minesweeper-get-face (val)
  "Gets the face for the character value of val. Proper inputs are ?0 through ?8, ?- and ?*"
  (gethash val *minesweeper-faces*))

(defun minesweeper-toggle-show-neighbors ()
  "Toggles whether neighbors are shown."
  (interactive)
  (if *minesweeper-idle-timer*
      (progn (cancel-timer *minesweeper-idle-timer*)
	     (minesweeper-reset-neighbor-overlays)
	     (setq *minesweeper-idle-timer* nil))
    (setq *minesweeper-idle-timer* (run-with-idle-timer *minesweeper-idle-delay*
							t
							'minesweeper-show-neighbors))))

(defun minesweeper-reset-neighbor-overlays ()
  "Move all the neighbor overlays to the beginning of the buffer. They won't be seen."
  (move-overlay *minesweeper-top-overlay* 0 0 (get-buffer "minesweeper"))
  (move-overlay *minesweeper-left-overlay* 0 0 (get-buffer "minesweeper"))
  (move-overlay *minesweeper-right-overlay* 0 0 (get-buffer "minesweeper"))
  (move-overlay *minesweeper-bottom-overlay* 0 0 (get-buffer "minesweeper")))

(defun minesweeper-in-bounds (x y)
  (minesweeper-debug "Called in-bounds with arguments " (number-to-string x) "\t" (number-to-string y) "\treturning " (if (and (< -1 x)
       (< x *minesweeper-board-width*)
       (< -1 y)
       (< y *minesweeper-board-height*)) "t" "nil"))
  (and (< -1 x)
       (< x *minesweeper-board-width*)
       (< -1 y)
       (< y *minesweeper-board-height*)))


