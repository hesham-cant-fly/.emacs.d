(defun config/load-all (root &rest names)
  (unless (stringp root)
	(error "Root is exepected to be a string"))
  (let ((root-path (expand-file-name root user-emacs-directory)))
    (dolist (name names)
      (unless (stringp name)
        (error (format "`%s` isn't a string. got `%s`"
                       name (type-of name))))
      (load-file (expand-file-name name root-path))))
  (elpaca-wait))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defun config/activate-lsp ()
  (interactive)
  (lsp))

(defun config/read-entire-file (FILE)
  "Reads `FILE' and returns its content as a string"
  (with-temp-buffer
	(-> FILE
		(expand-file-name user-emacs-directory)
		(insert-file-contents))
	(buffer-string)))

(config/load-all
 "lisp"
 "emacs-options.el"
 "bootstrap.el"
 "keybindings.el"
 "completions.el"
 "modals.el"
 "movement.el"
 "dashboard.el"
 "workspace.el"
 "dired-conf.el"
 "visuals.el"
 "ide.el"
 "org-mode-config.el"
 "git-integration.el")

(load-theme 'doom-rose-pine-moon t)

(setq-default c-style-alist '(("gnu" (c-basic-offset . 2) (c-comment-only-line-offset 0 . 0)
							   (c-hanging-braces-alist (substatement-open before after)
													   (arglist-cont-nonempty))
							   (c-offsets-alist (statement-block-intro . +) (knr-argdecl-intro . 5)
												(substatement-open . +) (substatement-label . 0)
												(label . 0) (statement-case-open . +)
												(statement-cont . +)
												(arglist-intro . c-lineup-arglist-intro-after-paren)
												(arglist-close . c-lineup-arglist)
												(inline-open . 0) (brace-list-open . +)
												(brace-list-intro first
																  c-lineup-2nd-brace-entry-in-arglist
																  c-lineup-class-decl-init-+ +)
												(topmost-intro-cont first
																	c-lineup-topmost-intro-cont
																	c-lineup-gnu-DEFUN-intro-cont))
							   (c-special-indent-hook . c-gnu-impose-minimum)
							   (c-block-comment-prefix . #1=""))
							  ("bsd" (c-basic-offset . tab-width) (c-comment-only-line-offset . 0)
							   (c-offsets-alist (statement-block-intro . +) (knr-argdecl-intro . +)
												(substatement-open . 0) (substatement-label . 0)
												(label . 0) (statement-cont . +) (inline-open . 0)
												(brace-list-intro first
																  c-lineup-2nd-brace-entry-in-arglist
																  c-lineup-class-decl-init-+ +)
												(inexpr-class . 0)))))

(add-to-list 'c-default-style
			 (cons #'c-mode "bsd"))

(config/load-all
 "modes"
 "haste-mode.el"
 "llvm-mode.el")

(put 'dired-find-alternate-file 'disabled nil)

(when (and custom-file (file-exists-p custom-file))
  (load custom-file))
