(defvar posframe-compilation--buf nil
  "")

(define-derived-mode posframe-compilation-mode compilation-mode "Compile Posframe"
  "Major Mode for `compilation-mode'"
  (general-def 'normal 'posframe-compilation-mode-map
	"q" #'delete-frame))

(defun posframe-compile (command)
  ""
  (interactive
   (list
	(let ((command (eval compile-command)))
	  (if (or compilation-read-command current-prefix-arg)
		  (compilation-read-command command)
		command))))
  (let* ((current-frame (selected-frame))
		 (fwidth (frame-width current-frame))
		 (fheight (frame-height current-frame))
		 (child-fwidth (round (/ fwidth 1.3)))
		 (child-fheight (round (/ fheight 1.3)))
		 (frame nil))
	(setq posframe-compilation--buf (compile command))
	(delete-windows-on posframe-compilation--buf)

	(setq frame (posframe-show
				 posframe-compilation--buf
				 :cursor 'box
				 :accept-focus t
				 :poshandler #'posframe-poshandler-frame-center
				 :override-parameters '((no-other-frame . t))
				 :width child-fwidth
				 :height child-fheight
				 :internal-border-width 1
				 :internal-border-color "gray"))

	(with-selected-frame frame
	  (select-frame-set-input-focus frame)
	  frame)))
