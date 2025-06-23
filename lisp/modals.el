
(use-package drag-stuff
	:ensure t
	:config
	(drag-stuff-global-mode 1)
	(drag-stuff-define-keys))

(use-package subword
	:hook
	(prog-mode . subword-mode)
	:general
	(config/leader-def
		:states 'normal
		"t s" '(subword-mode   :wk "Toggle subword-mode")))

(use-package superword
	:general
	(config/leader-def
		:states 'normal
		"t S" '(superword-mode :wk "Toggle superword-mode")))

(use-package evil-goggles
	:ensure t
	:after evil
	:config
	(evil-goggles-mode)
	(evil-goggles-use-diff-faces))

(use-package evil
	:ensure t
	:custom
	(evil-want-integration t)
	(evil-want-keybinding nil)
	:config
	(evil-mode 1)
	(evil-define-key '(normal visual motion) drag-stuff-mode-map
		(kbd "M-h") #'drag-stuff-left
		(kbd "M-j") #'drag-stuff-down
		(kbd "M-k") #'drag-stuff-up
		(kbd "M-l") #'drag-stuff-right))
(use-package evil-commentary
	:ensure t
	:after evil
	:config
	(evil-commentary-mode))
(use-package evil-exchange
	:ensure t
	:after evil
	:config
	(evil-exchange-install))
(use-package evil-surround
  :ensure t
	:after evil
  :config
  (global-evil-surround-mode 1))
(use-package evil-collection
	:ensure t
	:after evil
	:config
	(evil-collection-init))

(use-package god-mode
	:ensure t
	:commands (god-all-mode god-mode))

