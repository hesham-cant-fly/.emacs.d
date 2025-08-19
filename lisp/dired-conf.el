(use-package dired
	:custom
	(dired-listing-switches "-alh --group-directories-first")
	:general
	(:states 'normal :keymaps 'dired-mode-map
					 "h" '(dired-up-directory :wk "Up Directory")
					 "l" '(dired-find-alternate-file :wk "Open File")))

(use-package all-the-icons-dired
	:ensure t
	:if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
	:custom
	(all-the-icons-dired-monochrome nil))

(use-package sudo-edit
  :ensure t
  :general
  (config/leader-def
		:states 'normal
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit           :wk "Sudo this file")))
