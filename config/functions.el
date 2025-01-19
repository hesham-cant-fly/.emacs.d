
(defun get-selected-text (start end)
  (interactive "r")
  (if (use-region-p)
      (buffer-substring start end)))

(defun print-selected-text (start end)
  (interactive "r")
  (message (get-selected-text start end)))

(defun find-file-selected-text(start end)
  (interactive "r")
  (find-file (get-selected-text start end)))

; Reloading
(defun reload-init-file()
  (interactive)
  (load-file user-init-file))


(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defun rk/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or rk/copilot-manual-mode
      (member major-mode rk/no-copilot-modes)
      (company--active-p)))


