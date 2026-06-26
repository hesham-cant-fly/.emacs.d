;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5beb9cc517b24959e2ee7be47584270bbe11a7b210807fa419d41ede12174a26"
     "9b21c848d09ba7df8af217438797336ac99cbbbc87a08dc879e9291673a6a631"
     "fc1275617f9c8d1c8351df9667d750a8e3da2658077cfdda2ca281a2ebc914e0"
     "45631691477ddee3df12013e718689dafa607771e7fd37ebc6c6eb9529a8ede5"
     default))
 '(org-fold-catch-invisible-edits 'show-and-error nil nil "Customized with use-package org")
 '(package-selected-packages
   '(breadcrumb clojure-mode clojure-ts-mode dart-mode dockerfile-mode
                ef-themes scala-mode smalltalk-mode spacemacs-theme
                spacious-padding subatomic-theme toc-org tuareg
                yaml-mode zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-selection ((t (:background "#31748f" :foreground "white"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(highlight-doxygen-comment ((t (:inherit font-lock-doc-face :background "black"))))
 '(org-agenda-date ((t (:foreground "#c4a7e7"))))
 '(org-agenda-date-today ((t (:foreground "#f6c177" :weight bold))))
 '(org-agenda-date-weekend ((t (:foreground "#eb6f92"))))
 '(org-agenda-done ((t (:foreground "#9ccfd8"))))
 '(org-agenda-structure ((t (:foreground "#908caa" :weight bold))))
 '(org-block ((t (:inherit fixed-pitch :extend t))))
 '(org-checkbox ((t (:inherit fixed-pitch :weight bold))))
 '(org-checkbox-statistics-done ((t (:inherit fixed-pitch))))
 '(org-checkbox-statistics-todo ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit fixed-pitch :background "#26233a" :foreground "#9ccfd8"))))
 '(org-date ((t (:foreground "#c4a7e7" :underline t))))
 '(org-date-selected ((t (:inherit org-date :inverse-video t))))
 '(org-document-info ((t (:height 1.1 :foreground "#908caa"))))
 '(org-document-info-keyword ((t (:height 1.0 :foreground "#6e6a86"))))
 '(org-document-title ((t (:height 2.0 :weight bold))))
 '(org-done ((t (:weight bold :foreground "#9ccfd8"))))
 '(org-drawer ((t (:inherit fixed-pitch :foreground "#6e6a86"))))
 '(org-footnote ((t (:foreground "#eb6f92"))))
 '(org-formula ((t (:inherit fixed-pitch))))
 '(org-level-1 ((t (:inherit bold :foreground "#f6c177" :height 1.55))))
 '(org-level-2 ((t (:inherit bold :foreground "#ebbcba" :height 1.35))))
 '(org-level-3 ((t (:inherit bold :foreground "#9ccfd8" :height 1.2))))
 '(org-level-4 ((t (:inherit bold :foreground "#c4a7e7" :height 1.1))))
 '(org-level-5 ((t (:inherit bold :foreground "#eb6f92" :height 1.1))))
 '(org-level-6 ((t (:inherit bold :foreground "#31748f" :height 1.1))))
 '(org-level-7 ((t (:inherit bold :foreground "#f6c177" :height 1.1))))
 '(org-level-8 ((t (:inherit bold :foreground "#ebbcba" :height 1.1))))
 '(org-list-dt ((t (:weight bold))))
 '(org-meta-line ((t (:inherit fixed-pitch :foreground "#6e6a86"))))
 '(org-priority ((t (:weight bold))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-quote ((t (:inherit fixed-pitch :background "#1f1d2e" :foreground "#908caa" :slant italic))))
 '(org-scheduled ((t (:foreground "#e0def4"))))
 '(org-scheduled-previously ((t (:foreground "#eb6f92"))))
 '(org-scheduled-today ((t (:foreground "#f6c177"))))
 '(org-sexp-date ((t (:foreground "#c4a7e7"))))
 '(org-special-keyword ((t (:inherit fixed-pitch :foreground "#6e6a86"))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#e0def4"))))
 '(org-tag ((t (:weight bold :foreground "#908caa"))))
 '(org-time-grid ((t (:foreground "#6e6a86"))))
 '(org-todo ((t (:weight bold :foreground "#eb6f92"))))
 '(org-upcoming-deadline ((t (:foreground "#ebbcba"))))
 '(org-verbatim ((t (:inherit fixed-pitch :background "#26233a" :foreground "#c4a7e7"))))
 '(org-verse ((t (:background "#1f1d2e" :foreground "#908caa" :slant italic))))
 '(whitespace-lines-tail ((t (:background unspecified :foreground "#555555"))))
 '(whitespace-newline ((t (:background unspecified :foreground "#555555"))))
 '(whitespace-space ((t (:background unspecified :foreground "#555555"))))
 '(whitespace-tab ((t (:background unspecified :foreground "#555555"))))
 '(whitespace-trailing ((t (:background unspecified :foreground "#555555")))))
