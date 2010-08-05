(define-derived-mode minesweeper-mode nil "Minesweeper" "Major mode for playing Minesweeper in Emacs.
\\{minesweeper-mode-map}"
  (kill-all-local-variables)
  (use-local-map minesweeper-mode-map)
  (toggle-read-only t))

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

(defvar minesweeper-mines-left 0
  "Holds the number of mines left. After 'minesweeper-init has been called, the user will win the game when this becomes zero again.")

;; (defvar minesweeper-first-move 't
;;   "If 't, the next move is the first move. So if a mine is selected, move that mine elsewhere") ;; Do this later; it's less important

(defun minesweeper-init (&optional width height mines)
  "Begin a game of Minesweeper with a board that's 'width by 'height size containing 'mines mines."
  (setq minesweeper-board-width (or width minesweeper-default-width)
	minesweeper-board-height (or height minesweeper-default-height)
	minesweeper-mines (or mines minesweeper-default-mines)
	minesweeper-field (make-hash-table :test 'equal)
	minesweeper-reveals (make-hash-table :test 'equal)
	minesweeper-mines-left (- (* minesweeper-board-width
				     minesweeper-board-height)
				  minesweeper-mines))
  (minesweeper-fill-field)
  (minesweeper-print-field))


(defun minesweeper-fill-field ()
  "Fills 'minesweeper-field with 'minesweeper-mines mines, and builds the neighbor count."
  (for x 0 minesweeper-board-width
       (for y 0 minesweeper-board-height
	    (minesweeper-set-mine x y ?0)
	    (minesweeper-set-revealed x y nil)))
  (let ((mines-to-insert minesweeper-mines))
    (while (> mines-to-insert 0)
	   (let ((x (random minesweeper-board-width))
		 (y (random minesweeper-board-height)))
		 (unless (eq (minesweeper-view-mine x y)
			     ?X)
		   (minesweeper-set-mine x y ?X)
		   (minesweeper-inform-around x y)
		   (setq mines-to-insert (1- mines-to-insert)))))))

(defun minesweeper-view-mine (x y &optional reveal)
  "If reveal is true, or if the selected mine has been revealed, returns the value at position (x, y), where the origin is the upper left corner of the minefield. Otherwise, it returns '_'"
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
  "takes in a square, and increases the values of all its empty neighbors"
  (mapcar (lambda (position)
	    (apply 'minesweeper-++ position))
	  (minesweeper-neighbors x y)))

(defun minesweeper-inform-around (x y &opti)
  (mapcar (lambda (position)
	    (apply 'minesweeper-++ position))
	  (minesweeper-neighbors x y)))

(defun minesweeper-++ (x y &optional amount)
  ;; (minesweeper-set-mine x y 1))
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
    (for y 0 (1- minesweeper-board-height)
	 (for x 0 (1- minesweeper-board-width)
	      (insert-char (minesweeper-view-mine x y reveal) 1))
	 (newline))))

(defun minesweeper-to-character (val)
  "Takes in a number or ?X, and returns its printable character."
  val)
  ;; (cond ((eq val ?X)
  ;; 	 ?X)
  ;; 	((eq val ?_)
  ;; 	 ?_)
  ;; 	(t (+ ?0 val))))

(defun minesweeper-pick (x y &optional suppress-field)
  "Select the square at position (x, y) to reveal. A user-facing function."
  (insert "called pick " (+ ?0 x) " " (+ ?0 y))
  (newline)
  (unless (minesweeper-is-revealed x y)
    (let ((val (minesweeper-view-mine x y 't)))
      (if (eq val ?X)
	  (minesweeper-lose-game x y)
	(progn
	  (minesweeper-set-revealed x y 't)
	  (when (eq val ?0)
	    (minesweeper-pick-around x y))
	  (if (eq minesweeper-mines-left 0)
	      (mineweeper-win-game)
	    (unless suppress-field
	      (minesweeper-print-field))))))))


(defun minesweeper-pick-around (x y)
  "Pick all the squares around (x, y). As a precondition, (x, y) should be zero."
  (insert "called pick-around " (+ ?0 x) " " (+ ?0 y))
  (newline)
  (mapcar '(lambda (position) (insert "called pick-around-helper " (+ ?0 x) " " (+ ?0 y)) (newline) (minesweeper-pick (car position) (cadr position) 't))
	  (minesweeper-neighbors x y)))

(defun minesweeper-alternate-pick (x y) ;; DOES NOT WORK -- INFINITE LOOP!
  (unless (minesweeper-is-revealed x y)
    (let ((val (minesweeper-view-mine x y 't))
	  (stack (list (list x y))))
      (if (eq val ?X)
	  (minesweeper-lose-game x y)
	(progn (when (eq val ?0)
		 (while stack
		   (unless (apply 'minesweeper-is-revealed (car stack))
		     (minesweeper-set-revealed x y 't)
		     (mapcar (lambda (position) (push position stack))
			     (minesweeper-neighbors x y)))
		   (pop stack)))
	       (if (eq minesweeper-mines-left 0)
		   (minesweeper-win-game)
		 (minesweeper-print-field)))))))

(defun minesweeper-lose-game (x y)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "You lose. You chose spot (" (+ x ?0) ", " (+ y ?0) "), which was a bomb.")
    (newline 2)
    (minesweeper-print-field 't)))

(defun minesweeper-win-game ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (minesweeper-print-field 't)
    (newline 2)
    (insert "You win! Congrats!")))


(defmacro for (var init end &rest body)
  "helper function. executes 'body repeatedly, with 'var assigned values starting at 'init, and ending at 'end, increasing by one each iteration."
  `(let ((,var ,init)
	 (end-val ,end))
     (while (<= ,var end-val)
       ,@body
       (setq ,var (1+ ,var)))))




  ;; (run-hooks 'minesweeper-mode-hook))

;; (defvar minesweeper-mode-hook nil)

;; (defvar minesweeper-mode-map
;;   (let ((minesweeper-mode-map (make-sparse-keymap)))
;;     )
;;   "keymap for Minesweeper major mode")

