;; the prefix is config/

(when (and custom-file (file-exists-p custom-file))
  (load custom-file))

(defun config/load-all (root &rest names)
  (unless (stringp root)
    (error "Root is exepected to be a string"))
  (let ((root-path (expand-file-name root user-emacs-directory)))
    (dolist (name names)
      (unless (stringp name)
        (error (format "`%s` isn't a string. got `%s`"
                       name (type-of name))))
      (load-file (expand-file-name name root-path))))
  (elpaca-wait))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(config/load-all
 "lisp"
 "bootstrap.el"
 "emacs-options.el"
 "keybindings.el"
 "modals.el"
 "movement.el"
 "completions.el"
 "dashboard.el"
 "workspace.el"
 "dired-conf.el"
 "visuals.el"
 "ide.el"
 "org-mode-config.el"
 "git-integration.el"
 "universe.el")

(load-theme 'doom-dark+ t)
;; (load-theme 'timu-macos t)
(load-theme 'fleury t)

;; (load-theme 'gruvbox-dark-hard t)

(config/load-all
 "modes"
 "haste-mode.el")
(put 'dired-find-alternate-file 'disabled nil)
