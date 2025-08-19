
(use-package visual-fill-column
	:ensure t
	:custom
	(visual-fill-column-width 65)
	(visual-fill-column-center-text t)
	(visual-fill-column-finges-outside-margins t))

(use-package type-break
	;; :hook (after-init . type-break-mode)
	:custom
	(type-break-interval (* 30 60))
	(type-break-good-rest-interval (* 10 60))
	(type-break-good-break-interval (* 5 60))
	(type-break-query-mode t)
	(type-break-keystroke-threshold '(2100 . 3000))
	(type-break-demo-boring-stats t)
	(type-break-demo-functions '(type-break-demo-boring)))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode))

(use-package org
	:custom
	(org-directory (expand-file-name "~/Documents/org/"))
	(org-src-fontify-natively t)
	(org-fontify-whole-heading-line t)
	(org-fontify-done-headline t)
	(org-fontify-quote-and-verse-blocks t)
	(org-src-tab-acts-natively t)
	(org-src-window-setup 'current-window)
	(org-src-preserve-indentation t)
	(org-edit-src-content-indentation 0)
	(org-startup-with-inline-images t)
	(org-ellipsis "  [MORE]")
	(org-hide-emphasis-markers t)
	(org-link-descriptive t)
	(org-pretty-entities t)
	(org-hidden-keywords nil)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-agenda-tags-column 0)
  (org-startup-folded 'content)
	:hook
	(org-mode . visual-line-mode)
	(org-mode . visual-fill-column-mode)
	(org-mode . (lambda ()
								(setq display-line-numbers nil)))
	:general
	(config/leader-def
		:states 'normal
		"n"   '(:ignore t :wk "Org")
		"n t" '((lambda ()
							(interactive)
							(find-file (expand-file-name "~/Documents/org/refile.org")))
						:wk "Open refile.org"))
	:config
	(custom-set-faces
	 ;; Title styling
	 '(org-document-title ((t (:height 2.1 :weight bold))))

	 ;; Example of org-levels if you want them uncommented and customized manually
	 ;; '(org-level-1 ((t (:foreground "#00b6ef" :height 1.5 :weight bold :slant normal))))
	 ;; '(org-level-2 ((t (:foreground "#26fb91" :height 1.3 :weight bold :slant normal))))
	 ;; '(org-level-3 ((t (:foreground "#fb8a26" :height 1.15 :weight bold :slant normal))))

	 ;; Code block and quote background
	 '(org-code ((t (:background "#2e2e2e")))) ;; Replace "#2e2e2e" with a darkened bg
	 '(org-quote ((t (:background "#2e2e2e")))) ;; Same here
	 )
	(defun my/org-follow-link-or-return ()
		"Follow Org link in current window or execute default RET behavior."
		(interactive)
		(let ((context (org-element-context)))
			(if (and (listp context) (eq (org-element-type context) 'link))
					;; Temporarily change link behavior to use current window
					(let ((org-link-frame-setup '((file . find-file)
																				(vm . vm-visit-folder)
																				(wl . wl-other-frame)
																				(gnus . org-gnus-no-new-news)
																				(id . org-id-open)
																				(calendar . calendar))))
						(org-open-at-point))
				(org-return))))                 ; Default behavior
  
  ;; Bind to Enter in Evil normal state for Org-mode
  (with-eval-after-load 'org
    (with-eval-after-load 'evil
      (evil-define-key 'normal org-mode-map (kbd "RET") 'my/org-follow-link-or-return))))

(use-package org-appear
	:ensure t
	:after org
	:hook (org-mode . org-appear-mode)
	:custom
	(org-appear-autolinks t)
	(org-appear-autosubmarkers t)
	(org-appear-autoentities t)
	(org-appear-autokeywords t)
	(org-appear-inside-latex t))

(use-package org-superstar
  :ensure t
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-roam
	:ensure t
	:general
	(config/leader-def
		:states 'normal
		"n r"   '(:ignore t              :wk "Roam")
		"n r f" '(org-roam-node-find     :wk "Find Node")
		"n r r" '(org-roam-buffer-toggle :wk "Toggle Roam Buffer")
		"n r i" '(org-roam-node-insert   :wk "Insert a Node"))
	;; custom-set-faces
	:custom
  (org-roam-db-autosync-mode)
	(org-roam-directory (file-truename (expand-file-name org-directory "roam/")))
	(org-roam-capture-templates
	 '(
		 ("d" "Default" plain
			(file "~/Documents/org/roam/Templates/Default.org")
			:if-new
			(file+head "${slug}.org" "#+title: ${title}\n")
			:unnarrowed t)
		 ("s" "Source" plain
			(file "~/Documents/org/roam/Templates/Default.org")
			:if-new
			(file+head "Source/${slug}.org" "#+title: ${title}\n")
			:unnarrowed t)
		 ("l" "Linux" plain
			(file "~/Documents/org/roam/Templates/Default.org")
			:if-new
			(file+head "Linux/${slug}.org" "#+title: ${title}\n")
			:unnarrowed t)
		 ("p" "Projects" plain
			(file "~/Documents/org/roam/Templates/Default.org")
			:if-new
			(file+head "Projects/${slug}.org" "#+title: ${title}\n")
			:unnarrowed t)
		 ("h" "Haste Design Choices" plain
			(file "~/Documents/org/roam/Templates/Default.org")
			:if-new
			(file+head "LanguageDesign/${slug}.org" "#+title: ${title}\n")
			:unnarrowed t)
		 ("i" "Index" plain
			(file "~/Documents/org/roam/Templates/Default.org")
			:if-new
			(file+head "Index/${slug}.org" "#+title: ${title}\n")
			:unnarrowed t))))

(use-package org-roam-ui
	:ensure t
	:after org-roam)
