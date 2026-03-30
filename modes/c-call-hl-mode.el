(defvar c-call-font-lock-keywords
  `(
    ;; Function call: foo(...), foo <T> (...)
    (,(rx
       word-boundary
       (group (+ (or word ?_)))   ; function name
       (* space)
       (? (seq "<" (*? anything) ">")) ; optional <...>
       (* space)
       "(")
     1 font-lock-function-name-face))
  "Font-lock keywords for highlighting C-style function calls.")

(define-minor-mode c-call-hl-mode
  "Highlight C-style function calls."
  :lighter " CallHi"
  (if c-call-hl-mode
      (progn
        (font-lock-add-keywords nil c-call-font-lock-keywords 'append)
        (font-lock-flush))
    (font-lock-remove-keywords nil c-call-font-lock-keywords)
    (font-lock-flush)))
