(defvar my-shit-keyboard-fix-font-lock-keywords
  (let ((keywords '("get" "gt" "or" "and" "not")))
    `((,(regexp-opt keywords 'words)
       0 font-lock-keyword-face)))
  "Font-lock rules for my-shit-keyboard-fix-mode.")

(define-minor-mode my-shit-keyboard-fix-mode
  "A mode to fix my keyboard."
  :lighter " Fix"
  (if my-shit-keyboard-fix-mode
      (progn
        (font-lock-add-keywords nil my-shit-keyboard-fix-font-lock-keywords 'append)
        (font-lock-flush)
        (font-lock-ensure))
    (font-lock-remove-keywords nil my-shit-keyboard-fix-font-lock-keywords 'append)
    (font-lock-flush)
    (font-lock-ensure)))
