(defgroup paren-hide nil
  "Hide parentheses visually."
  :group 'editing)

(defvar paren-hide--keywords
  '(("\\([()]\\)"
     0
     (prog1 nil
       (put-text-property (match-beginning 0)
                          (match-end 0)
                          'invisible t))
     append)))

(defun paren-hide--enable ()
  (font-lock-add-keywords nil paren-hide--keywords 'append)
  (font-lock-flush))

(defun paren-hide--disable ()
  (font-lock-remove-keywords nil paren-hide--keywords)
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[()]" nil t)
        (remove-text-properties (match-beginning 0)
                                (match-end 0)
                                '(invisible)))))
  (font-lock-flush))

;;;###autoload
(define-minor-mode paren-hide-mode
  "Visually hide parentheses."
  :lighter " ()-"
  (if paren-hide-mode
      (paren-hide--enable)
    (paren-hide--disable)))
