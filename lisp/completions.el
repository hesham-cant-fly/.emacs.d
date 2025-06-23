;; (use-package vertico-flat
;; 	:after vertico
;; 	:config
;; 	(vertico-flat-mode))

(use-package vertico
	:ensure t
	:custom
	(vertico-count 15)
  (vertico-resize t)
  (vertico-cycle nil)
	:config
	(vertico-mode))

(use-package vertico-posframe
	:ensure t
	:config
	(vertico-posframe-mode 1))

(defun config/search-buffer ()
  "Search the current buffer using `consult-line`."
  (interactive)
	(let (start end multiline-p)
		(save-restriction
			(when (region-active-p)
				(setq start (region-beginning)
							end   (region-end)
							multiline-p (/= (line-number-at-pos start)
															(line-number-at-pos end)))
				(deactivate-mark)
				(when multiline-p
					(narrow-to-region start end)))
			(if (and start end (not multiline-p))
					(consult-line
                  (replace-regexp-in-string
                   " " "\\\\ "
                   (doom-pcre-quote
                    (buffer-substring-no-properties start end))))
				(call-interactively #'consult-line)))))

(use-package consult
	:ensure t
	:general
	(config/leader-def
		:states 'normal
		"s s" '(config/search-buffer :wk "Search Buffer")
    "s a" '(consult-ripgrep      :wl "Search All")))

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
