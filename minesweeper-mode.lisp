(define-derived-mode minesweeper-mode nil "Minesweeper" "Major mode for playing Minesweeper in Emacs.
\\{minesweeper-mode-map}"
  (kill-all-local-variables)
  (use-local-map minesweeper-mode-map)
  (toggle-read-only t))

(defvar minesweeper-board-width 10
  "The number of columns on the Minesweeper field.")

(defvar minesweeper-board-height 10
  "The number of rows on the Minesweeper field.")

(defvar minesweeper-mines 10
  "The number of mines on the Minesweeper field.")

(defvar minesweeper-field nil
  "The minefield itself. If a mine is in the square, 'X is stored. Otherwise, the number of mines in neighboring squares is stored.")

(defvar minesweeper-filter nil
  "Holds 't in (x, y) if (x, y) has been revealed")

(defun minesweeper-init ()
  "Begin a game of Minesweeper with a board that's 'width by 'height size containing 'mines mines."
  (setq ;; minesweeper-board-width width
	;; minesweeper-board-height height
	;; minesweeper-mines mines
   minesweeper-field (make-hash-table :test 'equal)
   minesweeper-filter (make-hash-table :test 'equal))
  (minesweeper-fill-field)
  (minesweeper-print-field))


(defun minesweeper-fill-field ()
  "Fills 'minesweeper-field with 'minesweeper-mines mines, and builds the neighbor count."
  (for x 0 minesweeper-board-width
       (for y 0 minesweeper-board-height
	    (minesweeper-set-mine x y 0)
	    (minesweeper-set-filter x y nil)))
  (let ((mines-to-insert minesweeper-mines))
    (while (> mines-to-insert 0)
	   (let ((x (random minesweeper-board-width))
		 (y (random minesweeper-board-height)))
		 (unless (eq (minesweeper-view-mine x y)
			     'X)
		   (minesweeper-set-mine x y 'X)
		   ;; (setq (minesweeper-view-mine x y)
		   ;;    'X)
		   ;; (minesweeper-inform-around x y)
		   (setq mines-to-insert (1- mines-to-insert)))))))

(defun minesweeper-view-mine (x y &optional reveal)
  "If reveal is true, or if the selected mine has been revealed, returns the value at position (x, y), where the origin is the upper left corner of the minefield. Otherwise, it returns '_'"
  (if (or reveal
	  (gethash (list x y)
		   minesweeper-filter))
      (gethash (list x y)
	       minesweeper-field)
    ?\_))

(defun minesweeper-set-mine (x y val)
  "Inserts val into the mine at (x, y)"
  (puthash (list x y)
	   val
	   minesweeper-field))

(defun minesweeper-set-filter (x y val)
  "Sets (x, y) in the filter to 'val"
  (puthash (list x y)
	   val
	   minesweeper-filter))

(defun minesweeper-inform-around (x y)
  "takes in a square, and increases the values of all its empty neighbors"
  (mapcar '(lambda (position) (apply minesweeper-++ position))
	  (minesweeper-neighbors x y)))

(defun minesweeper-++ (x y)
  "Increments the value at square (x, y), unless the square is a bomb"
  (let ((val (minesweeper-view-mine x y)))
    (unless (eq val 'X)
      (minesweeper-set-mine x
			    y
			    (1+ val)))))

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

(defun minesweeper-print-field ()
  "Print out the minefield."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (goto-char (point-min))
    (for x 0 (1- minesweeper-board-width)
	 (for y 0 (1- minesweeper-board-height)
	      (insert-char (minesweeper-to-character (minesweeper-view-mine x y)) 1))
	 (newline))))

(defun minesweeper-to-character (val)
  "Takes in a number or 'X, and returns its printable character."
  (cond ((eq val 'X)
	 ?X)
	((eq val ?_)
	 ?_)
	(t (+ ?0 val))))


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

