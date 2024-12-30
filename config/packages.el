;; -*- lexical-binding: t; -*-
(straight-use-package 'use-package)

(use-package diminish
  :straight t)
(use-package elcord
  :straight t
  :init (elcord-mode))
(use-package org
  :straight t
  :config
  (require 'org-tempo))
(use-package rainbow-mode
  :straight t
  :init (rainbow-mode))

;; Themes
(use-package autothemer
  :straight t)
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
  (evil-mode 1))
(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq-default evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor
  :straight t)

; I missed Telescope.. but!
(use-package counsel
  :straight t
  :diminish
  :after ivy
  :config (counsel-mode))
(use-package swiper
  :after ivy
  :straight t)
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

(use-package general ; Gotta read the docs
  :straight t
  :config
  (general-evil-setup)

  (general-define-key
   :states   '(normal visual emacs dashboard)
   "TAB"     'tab-line-switch-to-next-tab
   [backtab] 'tab-line-switch-to-prev-tab
   "S-TAB"   'tab-line-switch-to-prev-tab)

  ;; space key as the global leader key
  (general-create-definer hesham-cant-config/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; setting leader
    :global-prefix "M-SPC") ;; Meta key + space to access leader in insert mode
  ;; 'g' key as the global goto key
  (general-create-definer hesham-cant-config/goto-keys
    :states '(normal visual emacs)
    :prefix "g")

  ; Buffers
  (hesham-cant-config/leader-keys
   "b"    '(:ignore t                  :wk "Buffer")
   "bb"   '(switch-to-buffer           :wk "Switch buffer")
   "bi"   '(ibuffer                    :wk "IBuffer")
   "bk"   '(kill-this-buffer           :wk "Kill this buffer")
   "bn"   '(next-buffer                :wk "Next buffer")
   "bp"   '(previous-buffer            :wk "Previous buffer")
   "br"   '(revert-buffer              :wk "Reload buffer"))

  ; Telescope-like stuff
  (hesham-cant-config/leader-keys
   "f"    '(:ignore t                  :wk "Find")
   "fr"   '(counsel-recentf            :wk "Find recent files")
   "fc"   '((lambda ()
	      (interactive)
	      (find-file "~/.emacs.d/init.el"))
	    :wk "Edit emacs config")
   "ff"   '(counsel-find-file          :wk "Find File"))

  ; Evaluating Emacs lisp
  (hesham-cant-config/leader-keys
   "e"   '(:ignore t                  :wk "EShell/Evaluate")    
   "eb"  '(eval-buffer                :wk "Evaluate elisp in buffer")
   "ed"  '(eval-defun                 :wk "Evaluate defun containing or after point")
   "ee"  '(eval-expression            :wk "Evaluate and elisp expression")
   "el"  '(eval-last-sexp             :wk "Evaluate elisp expression before point")
   "er"  '(eval-region                :wk "Evaluate elisp in region")

   ; Eshel
   "eh"  '(counsel-esh-history        :wk "Eshell history")
   "es"  '(eshell                     :wk "Eshell"))

  ; Help Stuff
  (hesham-cant-config/leader-keys
   "h"   '(:ignore t                  :wk "Help")
   "hf"  '(describe-function          :wk "Describe function")
   "hv"  '(describe-variable          :wk "Describe variable")
   "hrr" '(reload-init-file           :wk "Reload emacs config"))

  ; Toggle Stuff
  (hesham-cant-config/leader-keys
   "t"   '(:ignore t                  :wk "Toggle")
   "tc"  '((lambda ()
             (interactive)
             (tab-line-close-tab t))
           :wk "Close tab and it's buffer")
   "tl"  '(display-line-numbers-mode  :wk "Toggle line numbers")
   "tt"  '(visual-line-mode           :wk "Toggle truncated lines")
   "tv"  '(vterm-toggle               :wk "Toggle vterm"))

  ; Window Stuff
  (hesham-cant-config/leader-keys
   "w"   '(:ignore t                  :wk "Windows")
   ;; Move Windows
   "wH"  '(buf-move-left              :wk "Buffer move left")
   "wJ"  '(buf-move-down              :wk "Buffer move down")
   "wK"  '(buf-move-up                :wk "Buffer move up")
   "wL"  '(buf-move-right             :wk "Buffer move right"))

  (hesham-cant-config/leader-keys
   "r"  '(:ignore                     :wk "Run")
   "rc" '(app-launcher-run-app        :wk "App launcher"))

  (hesham-cant-config/goto-keys
   "cc" '(comment-line               :wk "Comment lines")
   "l"  '(avy-goto-line              :wk "Quick line travel")
   "s"  '(avy-goto-char-timer        :wk "Avy mode")
   "r"  '(avy-resume                 :wk "Resume Avy")))

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
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

; App launcher
(use-package app-launcher
  :straight '(:host github :repo "SebastienWae/app-launcher"))

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
  :diminish
  :config
  (projectile-mode 1))

; Language support
(use-package lua-mode
  :straight t)
(use-package javascript-mode
  :straight t)
(use-package typescript-mode
  :straight t)
(use-package zig-mode
  :straight t)
(use-package rust-mode
  :straight t)

(use-package lsp-ui
  :straight t
  :ensure t)

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq-default lsp-keymap-prefix "C-c l")
  :config
  (setq-default lsp-zig-zls-executable "~/zls/zig-out/bin/zls")
  (lsp-enable-which-key-integration t)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'javascript-mode-hook #'lsp)
  (add-hook 'zig-mode-hook #'lsp-deferred))

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

;; (use-package corfu
;;   :straight '(corfu :type git :host github :repo "minad/corfu")
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-quit-at-coundary 'separator)
;;   (corfu-echo-documentation 0.25)
;;   (corfu-preview-current 'insert)
;;   (corfu-preselect-first nil)
;;   :bind (:map corfu-map
;;               ("M-TAB"      . corfu-insert-separator)
;;               ("RET"        . nil)
;;               ("TAB"        . corfu-next)
;;               ([tab]        . corfu-next)
;;               ("S-TAB"      . corfu-previous)
;;               ([backtab]    . corfu-previous)
;;               ("S-<return>" . corfu-insert))
;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   :config
;;   (add-hook 'eshell-mode-hook
;;             (lambda () (setq-local corfu-quit-at-boundary t
;;                                    corfu-quit-no-match t
;;                                    corfu-auto nil)
;;               (corfu-mode))))

;; (use-package emacs
;;   :custom
;;   (tab-always-indent 'complete)
;;   (text-mode-ispell-word-completion nil)
;;   (read-extended-command-predicate #'command-completion-default-include-p))

