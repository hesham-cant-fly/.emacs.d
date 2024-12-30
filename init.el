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
(load "keybindings")
(load "functions")
(load "launcher")

(custom-set-variables
  '(custom-safe-themes
    '("d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "4d16802de4686030ed8f30b5a844713d68edec9cc07322bef54493d15e68d8cd" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" default)))

(custom-set-faces)
