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

(defun minesweeper-init ()
  "Begin a game of Minesweeper with a board that's 'width by 'height size containing 'mines mines."
  (setq ;; minesweeper-board-width width
	;; minesweeper-board-height height
	;; minesweeper-mines mines
	minesweeper-field (make-hash-table :test eql)
  (minesweeper-fill-field)
  (minesweeper-display-field)))


(defun minesweeper-fill-field ()
  "Fills 'minesweeper-field with 'minesweeper-mines mines, and builds the neighbor count."
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

(defun minesweeper-view-mine (x y)
  "Returns the value at position (x, y), where the origin is the upper left corner of the minefield."
  (gethash (list x y)
	   minesweeper-field))

(defun minesweeper-set-mine (x y val)
  "Inserts val into the mine at (x, y)"
  (puthash (list x y)
	   val
	   minesweeper-field))

  ;; (kill-all-local-variables)
  ;; (use-local-map minesweeper-mode-map)
  ;; (run-hooks 'minesweeper-mode-hook))

;; (provide 'minesweeper-mode)

;; (defvar minesweeper-mode-hook nil)

;; (defvar minesweeper-mode-map
;;   (let ((minesweeper-mode-map (make-sparse-keymap)))
;;     )
;;   "keymap for Minesweeper major mode")

