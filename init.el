; (org-babel-load-file
;  (expand-file-name
;   "config.org"
;   user-emacs-directory))

; bootstraping Straight.el
(load-file "~/.emacs.d/bootstrap.el")

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq max-lisp-eval-depth 10000)

(load "packages")
(load "treesitter")
(load "buffers")
(load "options")
(load "functions")
(load "launcher")

(setenv "PATH" (concat "/home/hesham/.opam/default/bin:" (getenv "PATH")))

(require 'haste-mode)

(custom-set-variables
 '(custom-safe-themes
   '("b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" default))
 '(screenshot-line-numbers-p t))

(custom-set-faces
 '(dashboard-heading ((t (:foreground "cyan" :weight bold :height 0.8)))))
