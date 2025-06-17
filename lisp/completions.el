(use-package vertico
	:ensure t
	:custom
	(vertico-count 15)
  (vertico-resize t)
  (vertico-cycle nil)
	:config
	(vertico-mode))

;; (use-package vertico-flat
;; 	:after vertico
;; 	:config
;; 	(vertico-flat-mode))

(use-package vertico-mouse
	:after vertico
	:config
	(vertico-mouse-mode))

(use-package savehist
	:after vertico
	:init
	(savehist-mode))

(use-package orderless
	:ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
	:ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
	:ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package emacs
	:config
	(context-menu-mode t)
	(setq enable-recursive-minibuffers t
	      read-extended-command-predicate #'command-completion-default-include-p
	      minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))
