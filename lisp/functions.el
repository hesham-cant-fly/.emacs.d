;; Macros
(defmacro with-read-only (&rest body)
  "Execute BODY with `read-only-mode' temporarily enabled."
  `(let ((inhibit-read-only t))
	,@body))

(defmacro with-notabs (&rest body)
  "Execute BODY with `indent-tabs-mode' temporarily disabled.
Ensures the original value is restored even if an error occurs."
  (let ((old-value (make-symbol "old-value")))
	`(let ((,old-value indent-tabs-mode))
	   (unwind-protect
		   (progn
			 (setq indent-tabs-mode nil)
			 ,@body)
		 (setq indent-tabs-mode ,old-value)))))

(defmacro with-universal-argument (&rest body)
  "Execute BODY with the universal argument (C-u) set."
  (let ((uvalue '(list 4))
		(arguments (cddr body)))
	(if (keywordp (car body))
		(progn
		  (when (eq (car body) :value)
			(setf uvalue (cadr body))))
	  (setf arguments body))
	`(let ((current-prefix-arg ,uvalue))
	   ,@arguments)))

;; Functions
(defun compile-stdin ()
  "Run `compile' with a universal argument to read the command from stdin."
  (interactive)
  (with-universal-argument
   (call-interactively #'compile)))

(defun align-spaces-regexp ()
  "`align-regexp' but insures that it uses spaces instead of tabs"
  (interactive)
  (with-notabs
   (call-interactively #'align-regexp)))

(defun align-spaces ()
  "`align' but insures that it uses spaces instead of tabs"
  (interactive)
  (with-notabs
   (call-interactively #'align)))
