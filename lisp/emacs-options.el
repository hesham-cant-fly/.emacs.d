(setq-default
 ;; Editor Tweaks
 tab-width 2
 visible-bell t
 truncate-lines t
 indent-tabs-mode nil
 auto-save-default nil
 display-line-numbers-type 'relative
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 redisplay-dont-pause t
 scroll-margin 1
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1

 ;; Backup files tweaks
 make-backup-files nil
 create-lockfiles nil
 backup-directory-alist '((".*" . "~/.local/share/Trash/files"))

 auto-save-list-file-prefix nil)

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist
						 '(alpha-background . 100))

;; Setting up emacs fonts
(let ((font-size 150)
      (font-family "JetBrainsMono Nerd Font";;"DepartureMono Nerd Font Mono"
       ))
  (set-face-attribute 'fixed-pitch nil
                      :font font-family ;; "JetBrainsMono Nerd Font"
                      :height font-size)
  (set-face-attribute 'default nil
                      :font font-family ;; "JetBrainsMono Nerd Font"
                      :height font-size)
  (set-face-attribute 'variable-pitch nil
                      :font font-family ;; "JetBrainsMono Nerd Font"
                      :height font-size)
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
                      :slant 'italic))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(recentf-mode)

(add-hook #'prog-mode-hook (lambda ()
                             (interactive)
                             (display-line-numbers-mode)))
(defun my-force-tab-width ()
  "Force tab width to 2 and use spaces for indentation."
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(add-hook 'after-change-major-mode-hook 'my-force-tab-width)

(defalias (intern "toggle-read-only")
  `(lambda () (interactive) (read-only-mode)))

