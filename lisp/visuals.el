(use-package pdf-tools :ensure t)
(use-package tldr :ensure t)

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 1))

(use-package spacious-padding
  :ensure t
  :init ;; (spacious-padding-mode)
  )

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;"))
  (global-ligature-mode t))

(use-package keycast
  :ensure t
  :config
  ;;(keycast-mode-line-mode)
  )

(use-package writeroom-mode
  :ensure t
  :general
  (config/leader-def
    "t z" '(writeroom-mode :wk "Local Zen Mode")
    "t Z" '(global-writeroom-mode :wk "Global Zen Mode")))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

;; (use-package golden-ratio
;;   :ensure t
;;   ;; :after-call pre-command-hook
;;   :custom
;;   ;; (golden-ratio-adjust-factor 0.9)
;;   ;; (golden-ratio-wide-adjust-factor 0.9)
;;   (golden-ratio-max-width 100)
;;   (golden-ratio-auto-scale t)
;;   (golden-ratio-exclude-buffer-names '("*helm*" "*popup*" "*Messages*"))
;;   (golden-ratio-exclude-modes '(dired-mode))
;;   :hook (after-init . golden-ratio-mode)
;;   :general
;;   (:states 'normal
;;            "C-w o" '(golden-ratio :wk "")))

;;   ;; Hook into Evil's state changes
;;   ;; (add-hook 'evil-normal-state-entry-hook #'config/golden-ratio-evil-integration)
;;   ;; (add-hook 'evil-insert-state-entry-hook #'config/golden-ratio-evil-integration)
;;   ;; (add-hook 'evil-window-move-hook #'golden-ratio))

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

(use-package all-the-icons
	:ensure t
	:if (display-graphic-p))

(use-package rainbow-mode
	:ensure t)

(use-package hl-todo
	:ensure t
	:config
	(global-hl-todo-mode))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :ensure t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package highlight-blocks
  :ensure t)
  ;; :hook (prog-mode . highlight-blocks-mode))

(use-package hl-line) ;;(prog-mode . hl-line-mode)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package line-reminder
  :ensure t
  :config
  (global-line-reminder-mode t))

(use-package indent-bars
  :ensure t
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)))
  (indent-bars-treesit-wrap
   '((elisp
      quote special_form function_definition)))
  :config
  ;; Styles
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.15)
   indent-bars-pattern "."
   indent-bars-width-frac 0.15
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
   indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
   indent-bars-display-on-blank-lines t)
  :hook ((v-mode
          nix-mode
          tuareg-mode
          haskell-mode
          lua-mode
          d-mode
          cmake-mode
          emacs-lisp-mode
          scheme-mode
          haskell-mode
          zig-mode
          c-mode
          c++-mode) . indent-bars-mode))

;; (use-package centaur-tabs
;;   :ensure t
;;   :init
;;   (setq centaur-tabs-enable-key-bindings t)
;;   :config
;;   (setq centaur-tabs-style "bar"
;;         centaur-tabs-height 45
;;         centaur-tabs-set-icons t
;;         centaur-tabs-show-new-tab-button t
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-show-navigation-buttons t
;;         centaur-tabs-icon-type 'all-the-icons
;;         centaur-tabs-set-bar 'under
;;         centaur-tabs-show-count nil
;;         ;; centaur-tabs-label-fixed-length 15
;;         ;; centaur-tabs-gray-out-icons 'buffer
;;         ;; centaur-tabs-plain-icons t
;;         x-underline-at-descent-line t
;;         centaur-tabs-left-edge-margin nil)
;;   (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
;;   (centaur-tabs-headline-match)
;;   ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
;;   ;; (setq centaur-tabs-adjust-buffer-order t)
;;   ;; (centaur-tabs-mode t)
;;   (setq uniquify-separator "/")
;;   (setq uniquify-buffer-name-style 'forward)
;;   (defun centaur-tabs-buffer-groups ()
;;     "`centaur-tabs-buffer-groups' control buffers' group rules.

;; Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;; All buffer name start with * will group to \"Emacs\".
;; Other buffer group by `centaur-tabs-get-group-name' with project name."
;;     (list
;;      (cond
;;       ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
;;       ;; "Remote")
;;       ((or (string-equal "*" (substring (buffer-name) 0 1))
;;            (memq major-mode '(magit-process-mode
;;                               magit-status-mode
;;                               magit-diff-mode
;;                               magit-log-mode
;;                               magit-file-mode
;;                               magit-blob-mode
;;                               magit-blame-mode
;;                               )))
;;        "Emacs")
;;       ((derived-mode-p 'prog-mode)
;;        "Editing")
;;       ((derived-mode-p 'dired-mode)
;;        "Dired")
;;       ((memq major-mode '(helpful-mode
;;                           help-mode))
;;        "Help")
;;       ((memq major-mode '(org-mode
;;                           org-agenda-clockreport-mode
;;                           org-src-mode
;;                           org-agenda-mode
;;                           org-beamer-mode
;;                           org-indent-mode
;;                           org-bullets-mode
;;                           org-cdlatex-mode
;;                           org-agenda-log-mode
;;                           diary-mode))
;;        "OrgMode")
;;       (t
;;        (centaur-tabs-get-group-name (current-buffer))))))
;;   :hook
;;   (dashboard-mode . centaur-tabs-local-mode)
;;   (term-mode . centaur-tabs-local-mode)
;;   (calendar-mode . centaur-tabs-local-mode)
;;   (org-agenda-mode . centaur-tabs-local-mode)
;;   :bind
;;   ;; ("C-<prior>" . centaur-tabs-backward)
;;   ;; ("C-<next>" . centaur-tabs-forward)
;;   ;; ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
;;   ;; ("C-S-<next>" . centaur-tabs-move-current-tab-to-right)
;;   (:map evil-normal-state-map
;;         ("g t" . centaur-tabs-forward)
;;         ("g T" . centaur-tabs-backward)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-height 50
        doom-modeline-bar-width 5
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-total-line-number t))

;; (use-package spaceline
;;   :ensure t
;;   :custom
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

(use-package screenshot
  :ensure '(:host github :repo "tecosaur/screenshot")
  :after transient)

(use-package zenburn-theme
	:ensure t)

(use-package gruber-darker-theme
  :ensure t
  :config)

(use-package gruvbox-theme
  :ensure t
  :config)

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package wildcharm-theme
  :ensure t
  :init
  ;; (load-theme 'wildcharm t)
  :custom-face
  ;; org header sizes
  ;; (org-level-1 ((t (:height 1.6))))
  ;; (org-level-2 ((t (:height 1.4))))
  ;; (org-level-3 ((t (:height 1.2))))
  ;; italic comments
  (font-lock-comment-face ((t (:slant italic)))))

(use-package jetbrains-darcula-theme
	:ensure t)

