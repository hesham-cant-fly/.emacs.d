;; -*- lexical-binding: t; -*-
(straight-use-package 'use-package)

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

(use-package neotree
  :straight t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 30
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
  ;; truncate long file names in neotree
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
                (with-current-buffer (get-buffer neo-buffer-name)
                  (setq truncate-lines t)
                  (setq word-wrap nil)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq auto-hscroll-mode nil)))))

;; Themes
(use-package autothemer
  :straight t)
(use-package doom-themes
  :straight t
  :after all-the-icons
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-neotree-config)
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
  :after ivy)
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

(use-package projectile
  :straight t
  :ensure t
  :diminish
  :config
  (projectile-mode 1))

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
(use-package javascript-mode
  :straight t)
(use-package typescript-mode
  :straight t)
(use-package zig-mode
  :straight t)
(use-package rust-mode
  :straight t)


(use-package lsp-mode
  :straight t
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq-default lsp-keymap-prefix "C-c l")
  :hook ((
          c-mode
          c++-mode
          typescript-mode
          javascript-mode
          odin-mode
          lua-mode
          zig-mode) . lsp-deferred)
  :config
  (setq-default lsp-auto-guess-root t)

  (setq lsp-prefer-flymake nil)
  (setq-default lsp-zig-zls-executable "~/zls/zig-out/bin/zls")
  (setq-default lsp-clangd-binary-path "/usr/bin/clangd")
  (setq-default lsp-clients-lua-language-server-bin "/home/hesham/.local/share/nvim/mason/bin/lua-language-server")
  (setq lsp-language-id-configuration (cons '(odin-mode . "odin") lsp-language-id-configuration))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "/home/hesham/lsp/ols/ols") ;; Adjust the path here
                    :major-modes '(odin-mode)
                    :server-id 'ols
                    :multi-root t))
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :straight t
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable t))

;; (use-package corfu
;;   :straight t
;;   :init (global-corfu-mode))

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

