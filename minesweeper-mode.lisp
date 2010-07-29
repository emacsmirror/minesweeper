(defun minesweeper-mode ()
  "Major mode for playing Minesweeper in Emacs."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Minesweeper")
  (setq major-mode 'minesweeper-mode)
  (use-local-map minesweeper-mode-map)
  (run-hooks 'minesweeper-mode-hook))

(provide 'minesweeper-mode)

(defvar minesweeper-mode-hook nil)

(defvar minesweeper-mode-map
  (let ((minesweeper-mode-map (make-sparse-keymap)))
    )
  "keymap for Minesweeper major mode")

