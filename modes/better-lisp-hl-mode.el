(makunbound 'better-lisp-font-lock-keywords)

(defun better-lisp-keywords ()
  '("until" "while" "or" "and"))

(defvar better-lisp-font-lock-keywords
  `(
	(,(regexp-opt (better-lisp-keywords) 'symbols) . font-lock-keyword-face)
    (,(rx "("
          (zero-or-more space)
          (group
           (1+ (or letter digit "_" "-" "+" "*" "/" "<" ">" "=" "?")))
          symbol-end) 1 c-call-font-lock-keywords)
	(,(rx (or "*" "+")
          (group
           (1+ (or letter digit "_" "-" "+" "*" "/" "<" ">" "=" "?")))
		  (or "*" "+")) . font-lock-variable-name-face))
  "Highlight function names in Lisp-like forms.")

(defun better-lisp--enable ()
  (font-lock-add-keywords nil better-lisp-font-lock-keywords t)
  (font-lock-fontify-buffer))

(defun better-lisp--disable ()
  (font-lock-remove-keywords nil better-lisp-font-lock-keywords)
  (font-lock-fontify-buffer))

(define-minor-mode better-lisp-hl-mode
  "Highlight Lisp function calls."
  :lighter " CallHi"
  (if better-lisp-hl-mode
      (better-lisp--enable)
    (better-lisp--disable)))
