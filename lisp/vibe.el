(use-package gptel
  :ensure t
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-expert-commands t))

(use-package opencode
  :ensure '(:host github :repo "colobas/opencode.el" :branch "main")
  :after gptel
  :demand t
  :config
  (opencode-setup-coding)
  :general
  (config/leader-def
    "l o" '(opencode-setup       :wk "OpenCode Full")
    "l O" '(opencode-setup-coding :wk "OpenCode Coding")
    "l g" '(gptel                :wk "GPTel Chat")
    "l G" '(gptel-menu           :wk "GPTel Menu")))

(use-package evil-multiedit
  :ensure t
  :general
  (:keymaps 'override
   "C-h" '(evil-multiedit-match-and-prev    :wk "Mark previous like this")
   "C-l" '(evil-multiedit-match-and-next    :wk "Mark next like this"))
  )

(use-package evil-mc
  :ensure t
  :general
  (:keymaps 'override
   "C-k" '(evil-mc-make-and-goto-prev-match :wk "Advanced C-h")
   "C-j" '(evil-mc-make-and-goto-next-match :wk "Advanced C-l"))
  :config
  (global-evil-mc-mode))
