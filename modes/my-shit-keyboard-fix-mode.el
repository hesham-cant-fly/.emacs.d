(defvar my-shit-keyboard-fix-font-lock-keywords
  (let ((keywords '("get" "gt" "or" "and" "not"
					"discard" "forange" "leach" "arreach" "iarreach" "then" "otherwise"
					"defer" "errdefer"
					"run_at_percent"
					)))
    `((,(regexp-opt keywords 'words)
       0 font-lock-keyword-face prepend)))
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

;; (defvar my-shit-keyboard-fix-font-lock-keywords
;;   (let ((keywords '("get" "gt" "or" "and" "not"
;;                     "discard" "forange" "leach" "arreach" "iarreach" "then" "otherwise"
;;                     "defer"
;;                     "run_at_percent")))
;;     `((,(regexp-opt keywords 'words)
;;        0 font-lock-keyword-face prepend)))
;;   "Font-lock rules for my-shit-keyboard-fix-mode.")

;; (defun my-shit-keyboard-jit-fontify (beg end)
;;   "Directly apply keyword fixes over a region processed by jit-lock."
;;   (with-silent-modifications
;;     (save-excursion
;;       (font-lock-fontify-keywords-region beg end my-shit-keyboard-fix-font-lock-keywords)))
;;   ;; JIt-lock functions return nil or the list of updated regions
;;   nil)

;; (define-minor-mode my-shit-keyboard-fix-mode
;;   "A mode to fix my keyboard styling quirks by forcing specific keyword faces."
;;   :lighter " Fix"
;;   (if my-shit-keyboard-fix-mode
;;       ;; Register to run AFTER tree-sitter or major mode has updated the buffer
;;       (jit-lock-register #'my-shit-keyboard-jit-fontify)
;;     ;; Unregister when turned off
;;     (jit-lock-unregister #'my-shit-keyboard-jit-fontify))
  
;;   ;; Force Emacs to completely repaint the current buffer
;;   (when font-lock-mode
;;     (font-lock-flush)
;;     (font-lock-ensure)))


