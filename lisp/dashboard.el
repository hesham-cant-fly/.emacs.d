(use-package page-break-lines
  :ensure t
  :init (page-break-lines-mode))

;; hehhshsh
(use-package dashboard
  :ensure t
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))
