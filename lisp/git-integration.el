
(elpaca (transient :repo "magit/transient" :host github))
(use-package magit
	:ensure t)

(use-package git-gutter
	:ensure t
	:config
	(global-git-gutter-mode))

(use-package git-gutter-fringe
	:ensure t
	:after git-gutter+
	:config
	(git-gutter-fr-minimal))

