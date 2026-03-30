;; (indent-tabs-mode)
(setq-default
 ;; Editor Tweaks
 gc-cons-threshold 50000000
 x-stretch-cursor t
 tab-width 4
 visible-bell t
 truncate-lines t
 indent-tabs-mode t
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

(setq-default line-spacing 0)

;; Setting up emacs fonts
(let ((font-size 160)
      (font-family "0xProto Nerd Font Mono"))
  (set-face-attribute 'fixed-pitch nil
                      :font font-family
                      :height font-size)
  (set-face-attribute 'default nil
                      :font font-family
                      :height font-size)
  (set-face-attribute 'variable-pitch nil
                      :font font-family
                      :height font-size)
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
                      :slant 'italic)
  (set-fontset-font t 'arabic
                    (font-spec :family "Amiri" :size 16)))

(defun config/get-font-size ()
  "Returns the current font size"
  (face-attribute 'default :height))

(defun config/set-font-size (font-size)
  "Set the font size."
  (interactive
   (list
	(read-number (format "Font Size (currently %d): " (config/get-font-size)))))
  (let ((font-family "0xProto Nerd Font Mono"))
	(set-face-attribute 'fixed-pitch nil
						:font font-family
						:height font-size)
	(set-face-attribute 'default nil
						:font font-family
						:height font-size)
	(set-face-attribute 'variable-pitch nil
						:font font-family
						:height font-size)
	(set-face-attribute 'font-lock-comment-face nil
						:slant 'italic)
	(set-face-attribute 'font-lock-keyword-face nil
						:slant 'italic)
	(set-fontset-font t 'arabic
					  (font-spec :family "Amiri" :size 16))))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(recentf-mode 1)
(global-auto-revert-mode 1)

(setq-default c-basic-offset tab-width
              cperl-indent-level tab-width
              web-mode-code-indent-offset tab-width
              web-mode-markup-indent-offset tab-width
              web-mode-css-indent-offset tab-width
              js-indent-level tab-width
              rust-indent-offset tab-width
              zig-indent-offset tab-width
              nim-indent-offset tab-width
              ada-indent tab-width
              ada-use-indent tab-width
              ada-when-indent tab-width
              ada-with-indent tab-width
              ada-label-indent (- tab-width)
              hare-mode-indent-offset tab-width)
(add-hook #'prog-mode-hook
          (lambda ()
            (interactive)
            (display-line-numbers-mode)))

(defalias (intern "toggle-read-only")
  `(lambda () (interactive) (read-only-mode)))
