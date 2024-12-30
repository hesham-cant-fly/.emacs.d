
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
  (load-file user-init-file)
  (load-file user-init-file))

