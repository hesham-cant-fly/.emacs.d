(setenv "LSP_USE_PLISTS" "true")

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
 "functions.el"
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
 "git-integration.el"
 "vibe.el"
 "mode-line.el")

(load-theme 'kaolin-dark t)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

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
 "llvm-mode.el"
 "simpc-mode.el"
 "hare-mode.el"
 "c-call-hl-mode.el"
 "my-shit-keyboard-fix-mode.el")

(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(defun config/hook-c-highlight (&rest mode-hooks)
  ""
  (dolist (hook mode-hooks)
	(add-hook hook #'c-call-hl-mode)))

(defun config/hook-shit (&rest mode-hooks)
  ""
  (dolist (hook mode-hooks)
	(add-hook hook #'my-shit-keyboard-fix-mode)))

(config/hook-c-highlight
 'simpc-mode-hook
 'c-mode-hook
 'c++-mode-hook
 'rust-mode-hook
 'haste-mode-hook
 'java-mode-hook
 'hare-mode-hook)

(config/hook-shit
 ;; 'simpc-mode-hook
 'c-mode-hook
 'c++-mode-hook)

(put 'dired-find-alternate-file 'disabled nil)

;; (add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))

(when (and custom-file (file-exists-p custom-file))
  (load custom-file))
