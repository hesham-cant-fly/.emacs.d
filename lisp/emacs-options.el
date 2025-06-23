(setq-default
 ;; Editor Tweaks
 tab-width 2
 visible-bell t
 truncate-lines t
 indent-tabs-mode nil
 auto-save-default nil
 display-line-numbers-type 'relative
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 ;; Backup files tweaks
 make-backup-files nil
 create-lockfiles nil
 backup-directory-alist '((".*" . "~/.local/share/Trash/files"))

 auto-save-list-file-prefix nil)

(add-to-list 'default-frame-alist
						 '(alpha-background . 70))

;; Setting up emacs fonts
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono Nerd Font"
  :height 170)
(set-face-attribute 'default nil
  :font "JetBrainsMono Nerd Font"
  :height 170)
(set-face-attribute 'variable-pitch nil
  :font "JetBrainsMono Nerd Font"
  :height 170)
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(recentf-mode)
(display-line-numbers-mode)

(add-hook #'prog-mode (lambda ()
												(setq display-line-numbers 'relative)))

