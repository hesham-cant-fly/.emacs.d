;; haste-mode.el -- Major mode for editing Haste files

(defvar haste-mode-hook nil
  "Hook called when entering Haste mode.")

(defvar haste-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for Haste major mode.")

;;; Syntax highlighting
(defvar haste-font-lock-keywords
 `((,(regexp-opt '(
      "use" "import" "cimport" "mod"
      "struct" "enum" "variant" "error" "union" "type" "impl"
      "func" "!func"
      "let" "!let"
      "move" "mut" "try" "catch" "and" "or" "not" "in" "is" "as" "unsafe" "orelse"
      "if" "!if" "then" "else" "match" "case" "do"
      "for" "while" "loop" "skip" "stop"
      "return" "defer" "ret"
      "auto" "true" "false" "null") 'words) . font-lock-keyword-face)
    ("\\<\\(\\w+\\)\\s-*(" 1 font-lock-function-name-face)
    ("\\<\\([A-Z][A-Za-z0-9_]*\\|any\\|int\\|float\\|float64\\|uint\\|uint8\\|uint16\\|uint32\\|uint64\\|int8\\|int16\\|int32\\|int64\\|string\\|str\\|char\\|rune\\|isize\\|usize\\|bool\\)\\>" . font-lock-type-face))
  "Highlighting expressions for Haste mode.")

;;; Syntax table
(defvar haste-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat underscore as part of words
    (modify-syntax-entry ?_ "w" st)
    ;; Single-line comments starting with #
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for haste-mode.")

;;; Indentation
(defun haste-indent-line ()
  "Manually indent the current line by a fixed amount of spaces or tabs."
  (interactive)
  (let ((indent-size tab-width)) ;; Adjust `tab-width` as needed for spaces/tabs
    (if (eq (char-before) ?\n) ;; Indent only if at the start of a line
        (insert-char ?\s indent-size)
      (indent-line-to (+ (current-indentation) indent-size)))))

;;; Entry function
(defun haste-mode ()
  "Major mode for editing Haste language files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table haste-mode-syntax-table)
  (use-local-map haste-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(haste-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'haste-indent-line)
  (setq major-mode 'haste-mode)
  (setq mode-name "Haste")
  (run-hooks 'haste-mode-hook))

(provide 'haste-mode)

;; Automatically use haste-mode for .haste files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.haste\\'" . haste-mode))

(defun reload-haste-mode ()
  "Reload Haste mode for updated syntax highlighting."
  (interactive)
  (unload-feature 'haste-mode t)
  (haste-mode))

(provide 'reload-haste-mode)


