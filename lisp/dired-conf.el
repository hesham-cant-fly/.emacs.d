(use-package dired
	:custom
	(dired-listing-switches "-alh --group-directories-first")
	:general
	(:states 'normal :keymaps 'dired-mode-map
					 "h" '(dired-up-directory :which-key "Up Directory")
					 "l" '(dired-find-alternate-file :which-key "Open File")))

(use-package all-the-icons-dired
	:ensure t
	:if (display-graphic-p)
	:after (dired all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode)
	:custom
	(all-the-icons-dired-monochrome nil))
