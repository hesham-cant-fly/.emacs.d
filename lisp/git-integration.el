
(use-package transient
  :ensure t)
(use-package magit
	:ensure t)

(use-package git-gutter
	:ensure t
	:config
	(global-git-gutter-mode 1))

(use-package git-gutter-fringe
	:ensure t
	:after git-gutter+
	:config
	(git-gutter-fr-minimal 1))

