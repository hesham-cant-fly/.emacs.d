
(defcustom project-root-markers
  '("Cargo.toml" ".git" ".project" ".projectile")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(use-package project
  :ensure nil
  :config
  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers nil)
        (when (file-exists-p (concat path marker))
          (throw 'found marker)))))

  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (when-let ((root (locate-dominating-file path #'project-root-p)))
      (cons 'transient (expand-file-name root))))
  (setq project-find-functions '(project-find-root)))

(use-package emacs
	:custom
	(tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
	(read-extended-command-predicate #'command-completion-default-include-p))

(use-package glasses
	;; :hook (prog-mode . glasses-mode)
	:custom
	(glasses-separator "_")
	(glasses-separate-parentheses-p nil)
	(glasses-uncapitalize-p nil)
	:general
	(config/leader-def
		:states 'normal
		"t g" '(glasses-mode :wk "Toggles glasses-mode.")))

(use-package lsp-mode
  :ensure t
  :hook (((zig-mode
           haskell-mode
           web-mode
           ;; java-mode
           ;; c++-mode
           c-mode
           rust-mode
           go-mode
           python-mode
           js-mode
           typescript-mode
           ) . config/activate-lsp))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable t
        lsp-log-io nil)
  :config
  (setq-default lsp-odin-ols-binary-path "ols"
        lsp-odin-ols-server-dir "ols")
  (add-to-list 'exec-path "/home/hesham/.local/bin")
  (setq lsp-headerline-breadcrumb-enable nil ; Disable breadcrumb by default
        lsp-log-io nil      ; Disable logging (set to t for debugging)
        lsp-enable-snippet t
        lsp-idle-delay 0.500            ; Update delay (seconds)
        lsp-completion-provider :company
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting t)

  (setq lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server"
        lsp-clients-lua-language-server-main-location "/usr/lib/lua-language-server/main.lua")
  ;; (setq lsp-lua-workspace-library
  ;;       (list (expand-file-name "~/.config/love-api")))

  
  ;; Performance tuning
  (setq gc-cons-threshold (* 100000000 4))     ; 100MB
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  ;; (setq lsp-use-plists t)
  ;; :general
  ;; (:keymaps 'lsp-mode-map
  ;;           :state 'normal
  ;;           "K" '(lsp-describe-thing-at-point :wk "Describe"))
  :general
  (config/leader-def
    "l a" '(lsp-execute-code-action :wk "Code Actions")
    "l r" '(lsp-rename :wk "Rename")
    "l R" '(lsp-restart-workspace :wk "Restart workspace")
    "l f" '(lsp-format-buffer :wl "Format Buffer")))

;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-position 'at-point  ; top, bottom, or at-point
;;         lsp-ui-doc-show-with-cursor t
;;         lsp-ui-sideline-enable t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-diagnostics t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-imenu-enable t
;;         lsp-ui-peek-enable t)
;;   :general
;;   ;; Bind S-k to show hover
;;   (:states '(normal insert)
;;            :keymaps 'lsp-mode-map
;;            "K" 'lsp-ui-doc-glance))
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; Disable auto popup
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point)
  :general
  ;; Bind S-k to show hover
  (:states '(normal)
           :keymaps 'lsp-mode-map
           "K" 'lsp-ui-doc-toggle))

(use-package company-box
  :ensure t
  :config
  :hook (company-mode . company-box-mode))
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))
  ;; (global-set-key (kbd "C-SPC") 'company-complete))

(use-package flycheck
  :ensure t
  :hook (lsp-mode . flycheck-mode))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(defun config/treesit-parser-for-lang-mode (lang-mode-symbol)
  (when (and (treesit-available-p)
             (treesit-language-available-p lang-mode-symbol))
    (treesit-parser-create lang-mode-symbol)))

(use-package tree-sitter
  :ensure t
  :hook (((emacs-lisp-mode rust-mode scheme-mode haskell-mode lua-mode d-mode c-mode c++-mode js-mode typescript-mode) . tree-sitter-mode)
         ((emacs-lisp-mode rust-mode scheme-mode haskell-mode lua-mode d-mode c-mode c++-mode js-mode typescript-mode) . tree-sitter-hl-mode)
         ((emacs-lisp-mode) . (lambda () (config/treesit-parser-for-lang-mode 'elisp))))
  :config
  (setq-default treesit-language-source-alist
                '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                  (cmake "https://github.com/uyha/tree-sitter-cmake")
                  (css "https://github.com/tree-sitter/tree-sitter-css")
                  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                  (go "https://github.com/tree-sitter/tree-sitter-go")
                  (html "https://github.com/tree-sitter/tree-sitter-html")
                  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                  (json "https://github.com/tree-sitter/tree-sitter-json")
                  (make "https://github.com/alemuller/tree-sitter-make")
                  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                  (python "https://github.com/tree-sitter/tree-sitter-python")
                  (toml "https://github.com/tree-sitter/tree-sitter-toml")
                  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                  (clojure "https://github.com/sogaiu/tree-sitter-clojure")
                  (odin "https://github.com/ap29600/tree-sitter-odin")
                  (odin-mode "https://github.com/ap29600/tree-sitter-odin")
                  (c3 "https://github.com/c3lang/tree-sitter-c3")
                  (c3-ts-mode "https://github.com/c3lang/tree-sitter-c3")))
  )

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

(use-package fancy-compilation
  :ensure t
  :custom
  (fancy-compilation-quiet-prolog nil)
  :config
  ;;(fancy-compilation-mode)
  )

(use-package compile
  :ensure nil
  :hook
  (compilation-mode . config/compilation-mode-setup)
  :general
  (config/leader-def
    :state 'normal
    "p R" '(project-compile  :wk "Run Project")
    "c c" '(compile          :wk "Open Compilation Mode")
    "c r" '(recompile        :wk "Recompile"))
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-ask-about-save t)
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (compilation-read-command t)
  :config
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (defun config/jump-to-first-error (buffer status)
    "Move point to the first error in the compilation buffer if any."
    (interactive)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (ignore-errors
          (first-error)))))
  (add-hook 'compilation-finish-functions #'config/jump-to-first-error)

  (add-to-list 'display-buffer-alist
               '("\\*compilation\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3))))

(defun config/compilation-mode-setup ()
  "Custom settings for `compilation-mode`."
  (setq-local truncate-lines nil)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 101)
  (setq-local display-line-numbers nil)
  (setq-local global-hl-line-mode nil)
  (setq-local cursor-type nil)

  ;; Keybinding to quit buffer
  (local-set-key (kbd "q") #'kill-this-buffer)

  ;; Enable next/previous error navigation
  (local-set-key (kbd "n") #'next-error)
  (local-set-key (kbd "p") #'previous-error))

(use-package v-mode
  :ensure t
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode))

(use-package odin-mode
  :ensure '(:host github :repo "mattt-b/odin-mode")
  :hook (odin-mode . lsp))

(use-package qbe-mode
  :ensure '(:host github :repo "mbknust/qbe-mode"))

(use-package c3-ts-mode
  :ensure '(:host github :repo "c3lang/c3-ts-mode")
  ;; :hook (c3-ts-mode . lsp)
  )

(use-package nix-mode
	:ensure t
	:mode "\\.nix\\'")

(use-package elixir-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)
(use-package lsp-haskell
  :ensure t)

(use-package wren-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :hook (lua-mode . lsp))

(use-package zig-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :hook (go-mode . tree-sitter-hl-mode))

(use-package cmake-mode
  :ensure t)

(use-package d-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package nim-mode
  :ensure t)

(use-package forth-mode
  :ensure t)

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package clojure-mode
             :ensure t)
