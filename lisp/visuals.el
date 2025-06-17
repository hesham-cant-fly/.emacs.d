(use-package keycast
  :ensure t
  :config
  (keycast-mode-line-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package elcord
	:ensure t
	:config
	(elcord-mode))

(use-package rainbow-delimiters
	:ensure t
	:hook ((prog-mode) . rainbow-delimiters-mode))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package all-the-icons
	:ensure t
	:if (display-graphic-p))
