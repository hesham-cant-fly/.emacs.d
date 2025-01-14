; (org-babel-load-file
;  (expand-file-name
;   "config.org"
;   user-emacs-directory))

; bootstraping Straight.el
(load-file "~/.emacs.d/bootstrap.el")

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load "packages")
(load "buffers")
(load "options")
(load "functions")
(load "launcher")

(require 'haste-mode)

(custom-set-variables)

(custom-set-faces
 '(dashboard-heading ((t (:foreground "cyan" :weight bold :height 0.8)))))
