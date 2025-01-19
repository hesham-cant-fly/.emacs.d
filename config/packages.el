;; -*- lexical-binding: t; -*-
(straight-use-package 'use-package)

(use-package straight
  :custom
  ;; add project and flymake to the pseudo-packages variable so straight.el doesn't download a separate version than what eglot downloads.
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref))
  (straight-use-package-by-default t))

(use-package sublimity
  :straight t
  :init (sublimity-mode 1))
(use-package highlight-indent-guides
  :straight t
  :hook 
    ((prog-mode zig-mode haste-mode) . highlight-indent-guides-mode)
  :config 
  (setq-default highlight-indent-guides-method 'bitmap))

(use-package centaur-tabs
  :straight t
  :demand
  :config
  (setq centaur-tabs-style "wave")
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package editorconfig
  :straight t)
(use-package jsonrpc
  :straight t)
(use-package dash
  :straight t)
;; (use-package copilot
;;   :straight '(:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :ensure t
;;   :init (global-copilot-mode)
;;   :config
;;   (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate))
(use-package magit
  :straight t)
(use-package diminish
  :straight t)
(use-package elcord
  :straight t
  :init (elcord-mode))
(use-package org
  :straight t
  :init 
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook (lambda () (visual-line-mode t)))
  :config
  (require 'org-tempo)
  (electric-indent-mode -1))
(use-package toc-org
  :straight t
  :after org 
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))
(use-package org-bullets
  :straight t
  :after org
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package rainbow-delimiters
  :straight t
  :hook ((
          emacs-lisp-mode
          zig-mode
          ) . rainbow-delimiters-mode))
(use-package rainbow-mode
  :straight t
  :init (rainbow-mode))
(use-package tldr
  :straight t)
(use-package hl-todo
  :straight t
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package doom-modeline
  :straight t
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-height 40
        doom-modeline-bar-width 5
        doom-modeline-persp-name t
        doom-modeline-persp-icon t))

;; Dired
(use-package dired-open 
  :straight t
  :config 
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :straight t
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config 
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; (treemacs-start-on-boot)
;; (use-package neotree
;;   :straight t
;;   :config
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;   (setq neo-smart-open t
;;         neo-show-hidden-files t
;;         neo-window-width 30
;;         neo-window-fixed-size nil
;;         inhibit-compacting-font-caches t
;;         projectile-switch-project-action 'neotree-projectile-action) 
;;   ;; truncate long file names in neotree
;;   (add-hook 'neo-after-create-hook
;;             #'(lambda (_)
;;                 (with-current-buffer (get-buffer neo-buffer-name)
;;                   (setq truncate-lines t)
;;                   (setq word-wrap nil)
;;                   (make-local-variable 'auto-hscroll-mode)
;;                   (setq auto-hscroll-mode nil)))))

;; Themes
(use-package autothemer
  :straight t)
(use-package doom-themes
  :straight t
  :after all-the-icons
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (doom-themes-neotree-config)
  (doom-themes-org-config))
(use-package gruvbox-theme
  :straight t)
(use-package gruber-darker-theme
  :straight t)

;; Tree-sitter
(use-package tree-sitter
  :straight t
  :init
  (require 'tree-sitter)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :straight t
  :init
  (require 'tree-sitter-langs))

;; Evil-mode
(use-package evil
  :straight t
  :init
  (setq-default evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))
  ;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t))
(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq-default evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor
  :straight t)

; I missed Telescope.. but!
(use-package counsel-projectile
  :straight t
  :after counsel)
(use-package counsel
  :straight t
  :diminish
  :after ivy
  :config (counsel-mode))
(use-package swiper
  :after ivy
  :straight t)
(use-package ivy-rich
  :straight t
  :ensure t
  :after ivy
  :init (ivy-rich-mode 1)
  :config 
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
(use-package ivy
  :straight t
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))
(use-package all-the-icons-ivy-rich
  :straight t
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
(use-package ivy-rich
  :straight t
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

; I missed flash.nvim.. but!
(use-package avy
  :straight t)

(use-package general
  :straight t
  :config
  (general-evil-setup)
  (load "keybindings"))

; Which key
(use-package which-key
  :straight t
  :init (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 3
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator "    "))

(use-package sudo-edit
  :straight t
  :config
  (hesham-cant-config/leader-keys
    "fu" '(sudo-edit-find-file :wk "Sudo find file")
    "fU" '(sudo-edit           :wk "Sudo edit file")))

; Icons
(use-package all-the-icons
  :straight t
  :ensure t
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

; Shells
(use-package eshell-syntax-highlighting
  :straight t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package vterm
  :straight t
  :config
  (setq shell-file-name "/bin/fish"
	vterm-max-scrollback 5000))

(use-package vterm-toggle
  :straight t
  :after vterm
  :config
  (setq shell-file-name "/bin/fish")
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  (reusable-frames . visible)
                  (window-height . 0.3))))

(use-package screenshot
  :straight '(:host github :repo "tecosaur/screenshot")
  :ensure t)
; App launcher
(use-package app-launcher
  :straight '(:host github :repo "SebastienWae/app-launcher")
  :ensure t)

(use-package dashboard
  :straight t
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook)
  (custom-set-faces
   '(dashboard-heading ((t (:foreground "cyan" :weight bold :height 0.8))))))

; Language support
(use-package lua-mode
  :straight t
  :ensure t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
(use-package odin-mode
  :straight '(:host github :type git :repo "MrJCraft/odin-mode")
  :ensure t
  :mode "\\.odin\\'")
(use-package typescript-mode
  :straight t)
(use-package cc-mode
  :straight t)
(use-package zig-mode
  :straight t)
(use-package rust-mode
  :straight t)
(use-package go-mode
  :straight t)
(use-package svelte-mode
  :straight t)
(use-package sass-mode
  :straight t)
(use-package bnf-mode
  :straight t)
(use-package markdown-mode
  :straight t)

(use-package xref
  :straight t
  :config)
(use-package project
  :straight t
  :config)
(use-package projectile
  :straight t
  :ensure t
  :diminish
  :config
  (projectile-mode 1))
(use-package eglot
  :straight t
  :hook ((python-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (svelte-mode . eglot-ensure)
         (zig-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (odin-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("/home/hesham/.local/share/nvim/mason/bin/svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(typescript-mode . ("/home/hesham/.local/share/nvim/mason/bin/typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("/home/hesham/.local/share/nvim/mason/bin/pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(zig-mode . ("/home/hesham/zls/zig-out/bin/zls")))
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("/home/hesham/lsp/ols/ols" "--stdio")))
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rustup" "run" "stable" "rust-analyzer" :initializationOptions (:check (:command "clippy")))))
)

(use-package cape
  :straight t
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package grip-mode
  :straight t)

(use-package company
  :straight t
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :straight t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

