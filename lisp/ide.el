
(defcustom project-root-markers
  '("Cargo.toml" ".git" ".project" ".projectile")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(use-package project
	:config
	(defun project-root-p (path)
		"Check if the current PATH has any of the project root markers."
		(catch 'found
			(dolist (marker project-root-markers)
				(when (file-exists-p (concat path marker))
					(throw 'found marker)))))
	(defun project-find-root (path)
		"Search up the PATH for `project-root-markers'."
		(when-let ((root (locate-dominating-file path #'project-root-p)))
			(cons 'transient (expand-file-name root))))
	(setq project-find-functions '(project-find-root)))

(use-package corfu
	:ensure t
	:custom
	;; (global-corfu-modes '())
	(corfu-auto t)
	:init
	(global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package emacs
	:custom
	(tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
	(read-extended-command-predicate #'command-completion-default-include-p))

(use-package glasses
	:hook
	(prog-mode . glasses-mode)
	:custom
	(glasses-separator "_")
	(glasses-separate-parentheses-p nil)
	(glasses-uncapitalize-p nil)
	:general
	(config/leader-def
		:states 'normal
		"t g" '(glasses-mode :wk "Toggles glasses-mode.")))

(use-package eglot
  :hook ((
          c-mode
          c++-mode
          ) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs
               '(c++-mode . ("clangd"))))

(defun config/treesit-parser-for-lang-mode (lang-mode-symbol)
  (when (and (treesit-available-p)
             (treesit-language-available-p lang-mode-symbol))
    (treesit-parser-create lang-mode-symbol)))

(use-package tree-sitter
  :ensure t
  :hook (((emacs-lisp-mode scheme-mode) . tree-sitter-mode)
         ((emacs-lisp-mode scheme-mode) . tree-sitter-hl-mode)
         ((emacs-lisp-mode) . (lambda () (config/treesit-parser-for-lang-mode 'elisp)))))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package ebnf-mode
	:ensure t)

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode scheme-mode) . paredit-mode))

(use-package eros
  :ensure t
  :hook (emacs-lisp-mode . eros-mode))

(use-package v-mode
  :ensure t
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode))

(use-package nix-mode
	:ensure t
	:mode "\\.nix\\'")

(use-package cmake-mode
  :ensure t)

