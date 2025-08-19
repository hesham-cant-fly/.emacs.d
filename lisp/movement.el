(use-package avy
	:ensure t
	:after evil
	:config
	(evil-define-key '(normal visual motion) 'global
		(kbd "g l") #'avy-goto-line
		(kbd "s") #'avy-goto-char-2-below
		(kbd "S-s") #'avy-goto-char-2-above))
