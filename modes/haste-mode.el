(eval-when-compile
	(require 'rx))

(defconst haste--font-lock-defaults
	(let ((keywords
				 '(
           "owned"
           "context" "new" "delete"
           "interface" "class" "!class" "enum" "variant" "error"
           "static" "import" "pub" "!pub" "use"
					 "var" "!var" "func" "!func"
           "match" "case" "!case" "if" "!if" "else" "do" "then"
           "for" "while" "skip" "stop"
           "in" "is" "or" "and" "not"
           "try" "catch"
           "return" "defer" "errdefer"
           "true" "false" "null"
					 )
				 )
				(types
				 '(
           "auto" "void"
					 "char" "str" "string"
					 "bool"
           "uint" "uint8" "uint16" "uint32" "uint64" "int" "int8" "int16" "int32" "int64" "usize" "isize"
           "float" "float64" "fsize"
           "self"
					 )))
;;(rx-to-string `(: (or ,@keywords)))
		`(((,(regexp-opt keywords 'words) 0 font-lock-keyword-face)
			 ("\\([[:word:]]+\\)\s*(" 1 font-lock-function-name-face)
       ;; ("[[:word:]]+" 1 font-lock-variable-name-face)
			 (,(regexp-opt types 'words) 0 font-lock-type-face)))))


(defvar haste-mode-syntax-table
	(let ((st (make-syntax-table)))
		(modify-syntax-entry ?\{ "(}" st)
		(modify-syntax-entry ?\} "){" st)
		(modify-syntax-entry ?\( "()" st)

		;; Word
		(modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?! "w" st)
    (modify-syntax-entry ?$ "w" st)
    (modify-syntax-entry ?@ "w" st)

    ;; both single and double quotes makes strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)

    ;; add comments.
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)

    ;; '==' as punctuation
    (modify-syntax-entry ?= "." st)
    st))

(defun haste-indent-line ()
	"Indent current line."
	(let (indent
				boi-p                           ;begin of indent
				move-eol-p
				(point (point)))                ;lisps-2 are truly wonderful
		(save-excursion
			(back-to-indentation)
			(setq indent (car (syntax-ppss))
						boi-p (= point (point)))
			;; don't indent empty lines if they don't have the in it
			(when (and (eq (char-after) ?\n)
								 (not boi-p))
				(setq indent 0))
			;; check whether we want to move to the end of line
			(when boi-p
				(setq move-eol-p t))
			;; decrement the indent if the first character on the line is a
			;; closer.
			(when (or (eq (char-after) ?\))
								(eq (char-after) ?\}))
				(setq indent (1- indent)))
			;; indent the line
			(delete-region (line-beginning-position)
										 (point))
			(indent-to (* tab-width indent)))
		(when move-eol-p
			(move-end-of-line nil))))


(defvar haste-mode-abbrev-table nil
	"Abbreviation table used in `nps-mode' buffers.")

(define-abbrev-table 'haste-mode-abbrev-table
	'())

;;;###autoload
(define-derived-mode haste-mode prog-mode "haste"
  "Major mode for haste files."
  :abbrev-table haste-mode-abbrev-table
  (setq font-lock-defaults haste--font-lock-defaults)
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local indent-line-function #'haste-indent-line)
  (setq-local indent-tabs-mode t))

(add-to-list 'auto-mode-alist '("\\.haste" . haste-mode))

(defvar haste-mode-map
  (let ((map (make-sparse-keymap)))
    map))
