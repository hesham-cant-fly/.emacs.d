(use-package pdf-tools
  :ensure t
  )
(use-package tldr
  :ensure t
  )

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
  :ensure t
  )


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
  :ensure t
  ;; :hook (prog-mode . highlight-blocks-mode)
  )

(use-package hl-line
  :ensure t
  :hook (prog-mode . hl-line-mode))

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
  (indent-bars-treesit-wrap
   '((elisp quote special_form function_definition)
	 (c argument_list parameter_list init_declarator parenthesized_expression)))
  :config
  ;; Styles
  (setq-default
   indent-bars-color '(highlight :face-bg t :blend 0.15)
   indent-bars-pattern ". . . ."
   indent-bars-width-frac 0.15
   indent-bars-pad-frac 0.2
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
   indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
   indent-bars-display-on-blank-lines t)
  :hook ((prog-mode) . indent-bars-mode))

(use-package doom-modeline
  :ensure t
  ;; :init (doom-modeline-mode t)
  :config
  (setq-default doom-modeline-height 50
        doom-modeline-bar-width 5
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-total-line-number t))

(use-package nyan-mode
  :ensure t
  :hook ((prog-mode text-mode) . nyan-mode)
  :config
  (setq-default nyan-animate-nyancat t)
  (nyan-start-animation))

(use-package screenshot
  :ensure '(:host github :repo "tecosaur/screenshot")
  
  :after transient)

(use-package zenburn-theme
  :ensure t
  )

(use-package gruber-darker-theme
  :ensure t
  
  :config)

(use-package gruvbox-theme
  :ensure t
  
  :config)

(use-package wildcharm-theme
  :ensure t
  
  :custom-face (font-lock-comment-face ((t (:slant italic)))))

(use-package jetbrains-darcula-theme
  :ensure t
  )

(use-package kaolin-themes
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))
