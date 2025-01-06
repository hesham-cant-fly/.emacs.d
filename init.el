; (org-babel-load-file
;  (expand-file-name
;   "config.org"
;   user-emacs-directory))

; bootstraping Straight.el
(load-file "~/.emacs.d/bootstrap.el")

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load "packages")
(load "buffers")
(load "options")
(load "functions")
(load "launcher")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-heading ((t (:foreground "cyan" :weight bold :height 0.8)))))
