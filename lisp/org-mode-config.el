(use-package htmlize
  :ensure t)

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-width 90)
  (visual-fill-column-center-text t)
  (visual-fill-column-fringes-outside-margins t))

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
  :custom
  (org-modern-keyword t)
  (org-modern-todo t)
  (org-modern-priority t)
  ;; (org-modern-checkbox t)
  ;; (org-modern-hide-stars t)
  (org-modern-star '("◉" "○" "✸" "✿" "◆" "▷"))
  (org-modern-list '((?- . "•") (?+ . "‣") (?* . "◦")))
  (org-modern-block-name t)
  (org-modern-table t)
  (org-modern-timestamp nil)
  (org-modern-variable-pitch nil)
  :hook
  ;; (org-mode . org-modern-mode)
  ;; (org-agenda-finalize . org-modern-agenda)
  )

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
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  (org-link-descriptive t)
  (org-pretty-entities t)
  (org-hidden-keywords nil)
  (org-auto-align-tags nil)
  (org-tags-column -80)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-agenda-tags-column -100)
  (org-startup-folded 'content)
  (org-cycle-emulate-tab 'white)
  (org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  :hook
  (org-mode . visual-line-mode)
  (org-mode . visual-fill-column-mode)
  (org-mode . org-indent-mode)
  (org-mode . (lambda ()
                (setq display-line-numbers nil)
                (setq line-spacing 0.2)))
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
   '(org-document-title ((t (:height 2.0 :weight bold))))
   '(org-document-info ((t (:height 1.1 :foreground "#908caa"))))
   '(org-document-info-keyword ((t (:height 1.0 :foreground "#6e6a86"))))

   '(org-level-1 ((t (:inherit bold :foreground "#f6c177" :height 1.55))))
   '(org-level-2 ((t (:inherit bold :foreground "#ebbcba" :height 1.35))))
   '(org-level-3 ((t (:inherit bold :foreground "#9ccfd8" :height 1.2))))
   '(org-level-4 ((t (:inherit bold :foreground "#c4a7e7" :height 1.1))))
   '(org-level-5 ((t (:inherit bold :foreground "#eb6f92" :height 1.1))))
   '(org-level-6 ((t (:inherit bold :foreground "#31748f" :height 1.1))))
   '(org-level-7 ((t (:inherit bold :foreground "#f6c177" :height 1.1))))
   '(org-level-8 ((t (:inherit bold :foreground "#ebbcba" :height 1.1))))

   '(org-code ((t (:inherit fixed-pitch :background "#26233a" :foreground "#9ccfd8"))))
   '(org-verbatim ((t (:inherit fixed-pitch :background "#26233a" :foreground "#c4a7e7"))))
   '(org-quote ((t (:inherit fixed-pitch :background "#1f1d2e" :foreground "#908caa" :slant italic))))
   '(org-verse ((t (:background "#1f1d2e" :foreground "#908caa" :slant italic))))

   '(org-block ((t (:inherit fixed-pitch :extend t))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#e0def4"))))
   '(org-formula ((t (:inherit fixed-pitch))))
   '(org-special-keyword ((t (:inherit fixed-pitch :foreground "#6e6a86"))))
   '(org-property-value ((t (:inherit fixed-pitch))))
   '(org-meta-line ((t (:inherit fixed-pitch :foreground "#6e6a86"))))
   '(org-drawer ((t (:inherit fixed-pitch :foreground "#6e6a86"))))

   '(org-checkbox ((t (:inherit fixed-pitch :weight bold))))
   '(org-checkbox-statistics-todo ((t (:inherit fixed-pitch))))
   '(org-checkbox-statistics-done ((t (:inherit fixed-pitch))))

   '(org-tag ((t (:weight bold :foreground "#908caa"))))
   '(org-list-dt ((t (:weight bold))))
   '(org-footnote ((t (:foreground "#eb6f92"))))

   '(org-date ((t (:foreground "#c4a7e7" :underline t))))
   '(org-date-selected ((t (:inherit org-date :inverse-video t))))
   '(org-time-grid ((t (:foreground "#6e6a86"))))
   '(org-sexp-date ((t (:foreground "#c4a7e7"))))

   '(org-todo ((t (:weight bold :foreground "#eb6f92"))))
   '(org-done ((t (:weight bold :foreground "#9ccfd8"))))
   '(org-priority ((t (:weight bold))))

   '(org-agenda-date ((t (:foreground "#c4a7e7"))))
   '(org-agenda-date-today ((t (:foreground "#f6c177" :weight bold))))
   '(org-agenda-date-weekend ((t (:foreground "#eb6f92"))))
   '(org-agenda-done ((t (:foreground "#9ccfd8"))))
   '(org-scheduled ((t (:foreground "#e0def4"))))
   '(org-scheduled-today ((t (:foreground "#f6c177"))))
   '(org-scheduled-previously ((t (:foreground "#eb6f92"))))
   '(org-upcoming-deadline ((t (:foreground "#ebbcba"))))
   '(org-agenda-structure ((t (:foreground "#908caa" :weight bold))))
   ))
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
    (evil-define-key 'normal org-mode-map (kbd "RET") 'my/org-follow-link-or-return)))

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
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "◆" "▷"))
  (org-superstar-item-bullet-alist '((?* . "•") (?+ . "‣") (?- . "◦")))
  (org-superstar-special-todo-items t)
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  )

(use-package org-roam
  :ensure t
  :demand t
  :general
  (config/leader-def
    :states 'normal
    "n r"   '(:ignore t              :wk "Roam")
    "n r f" '(org-roam-node-find     :wk "Find Node")
    "n r r" '(org-roam-buffer-toggle :wk "Toggle Roam Buffer")
    "n r i" '(org-roam-node-insert   :wk "Insert a Node"))
  ;; custom-set-faces
  :custom
  (org-roam-directory (expand-file-name "roam/" org-directory))
  (org-roam-capture-templates
   '(("d" "Default" plain
      (file "~/Documents/org/roam/Templates/Default.org")
      :if-new
      (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("s" "Resources" plain
      (file "~/Documents/org/roam/Templates/Default.org")
      :if-new
      (file+head "Resources/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("c" "Computer Science" plain
      (file "~/Documents/org/roam/Templates/Default.org")
      :if-new
      (file+head "ComputerScience/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("h" "Haste Design Choice" plain
      (file "~/Documents/org/roam/Templates/Default.org")
      :if-new
      (file+head "LanguageDesign/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("u" "Unix" plain
      (file "~/Documents/org/roam/Templates/Default.org")
      :if-new
      (file+head "Unix/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("t" "Topic" plain
      (file "~/Documents/org/roam/Templates/Default.org")
      :if-new
      (file+head "Topics/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :ensure t
  :after org-roam)

(use-package org-download
  :ensure t
  :after org
  :config
  (defun config/setup-org-download-for-org-roam ()
    "Set's up `org-download-image-dir' for org-roam"
    (interactive)
    (when (string-prefix-p (expand-file-name org-roam-directory)
                           (expand-file-name default-directory))
      (setq-local org-download-image-dir (expand-file-name "Figures/" org-roam-directory))))

  (add-hook 'org-mode-hook #'config/setup-org-download-for-org-roam)
  (add-hook 'dired-mode-hook 'org-download-enable))

