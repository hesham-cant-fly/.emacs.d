;; (use-package gptel
;;   :ensure t
;;   :custom (gptel-api-key #'gptel-api-key-from-auth-source)
;;   :config
;;   (gptel-make-gemini "Gemini" :key #'gptel-api-key :stream t))
; (use-package copilot
;   :ensure '(:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;   :hook (prog-mode . copilot-mode)
;   :custom
;   (copilot-idle-delay 0.1)
;   :config
;   (add-to-list 'copilot-indentation-alist '(prog-mode tab-width))
;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode tab-width))
;   (add-to-list 'copilot-indentation-alist '(c-mode tab-width))
;   (add-to-list 'copilot-indentation-alist '(simpc-mode tab-width))
;   (add-to-list 'copilot-indentation-alist '(c++-mode tab-width))
;
;   (define-key copilot-mode-map (kbd "M-TAB") 'copilot-accept-completion))
;
; (use-package copilot-chat
;   :ensure t)
