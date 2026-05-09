(defgroup paren-hidden nil
  "Hide parentheses by matching background color."
  :group 'faces)

(defface paren-hidden-face
  '((t :inherit background))
  "Face that hides parentheses.")

(defun paren-hidden--update-face ()
  (let ((bg (face-background 'default nil t)))
    (set-face-attribute 'paren-hidden-face nil :foreground bg)))

(defvar paren-hidden--keywords
  '(("[()]" . 'paren-hidden-face)))

;;;###autoload
(define-minor-mode paren-hidden-mode
  "Minor mode to hide parentheses by coloring them like background."
  :lighter " ()x"
  (if paren-hidden-mode
      (progn
        (paren-hidden--update-face)
        (font-lock-add-keywords nil paren-hidden--keywords 'append))
    (font-lock-remove-keywords nil paren-hidden--keywords))
  (font-lock-flush))

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
  (font-lock-flush)
  (font-lock-ensure))

(defun paren-hide--disable ()
  (font-lock-remove-keywords nil paren-hide--keywords)
  (font-lock-flush)
  (font-lock-ensure))

;;;###autoload
(define-minor-mode paren-hide-mode
  "Visually hide parentheses."
  :lighter " ()-"
  (if paren-hide-mode
      (paren-hide--enable)
    (paren-hide--disable)))
