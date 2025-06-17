(use-package avy
	:ensure t
	:after evil
	:config
	(evil-define-key '(normal visual motion) 'global
		(kbd "g l") #'avy-goto-line
		(kbd "f") #'avy-goto-char-2-below
		(kbd "S-f") #'avy-goto-char-2-above))
