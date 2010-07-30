(define-derived-mode minesweeper-mode nil "Minesweeper" "Major mode for playing Minesweeper in Emacs.
\\{minesweeper-mode-map}"
  (kill-all-local-variables)
  (use-local-map minesweeper-mode-map)
  (toggle-read-only t))

(defvar minesweeper-board-width nil
  "The number of columns on the Minesweeper field.")

(defvar minesweeper-board-height nil
  "The number of rows on the Minesweeper field.")

(defvar minesweeper-mines nil
  "The number of mines on the Minesweeper field.")

(defun minesweeper-init (width height mines)
  "Begin a game of Minesweeper with a board that's 'width by 'height size containing 'mines mines."
  (setq minesweeper-board-width width
	minesweeper-board-height height
	minesweeper-mines mines)
  (fill-minefield))


  ;; (kill-all-local-variables)
  ;; (use-local-map minesweeper-mode-map)
  ;; (run-hooks 'minesweeper-mode-hook))

;; (provide 'minesweeper-mode)

;; (defvar minesweeper-mode-hook nil)

;; (defvar minesweeper-mode-map
;;   (let ((minesweeper-mode-map (make-sparse-keymap)))
;;     )
;;   "keymap for Minesweeper major mode")

