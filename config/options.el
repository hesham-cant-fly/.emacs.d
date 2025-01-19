;; Settings
(setq-default auto-save-list-file-prefix nil) ; Something related to ~/.emacs.d/auto-safe-list i think
(setq-default display-line-numbers-type 'relative) ; relative numbers :3
(setq-default truncate-lines t) ; Let's disable line wraping :D
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default shell-file-name "/bin/fish")
(setq-default make-backup-files nil) ; no "name~" type of shit any more!!!
(setq-default create-lockfiles nil)
(setq-default auto-save-default nil)
(setq backward-delete-char-untabify-method nil)
(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; Transparency

(load-theme 'doom-acario-dark t)

; Gotta remeber how amazing the eval-region command is!!
(set-face-attribute 'fixed-pitch nil ; Emacs font
  :font "JetBrainsMono Nerd Font"
  :height 130)
(set-face-attribute 'default nil
  :font "JetBrainsMono Nerd Font"
  :height 130)
(set-face-attribute 'variable-pitch nil
  :font "Noto Sans Mono"
  :height 130)

(set-face-attribute 'font-lock-comment-face nil ; WOW!
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font"))

(global-display-line-numbers-mode t) ; Activates line numbers on emacs
(global-visual-line-mode -1) ; Disabling line wraping (again)

;; (global-tab-line-mode 0) ; Activates the tab line
(menu-bar-mode 0) ; Disalble the menu bar
(tool-bar-mode 0) ; Kinda the same
(scroll-bar-mode 0) ; No scroll bar

; EShell
(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("fish" "bash" "htop" "ssh" "top" "zsh"))

; Sublimity
(require 'sublimity-scroll)
