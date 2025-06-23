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
      (load-file (expand-file-name name root-path)))))

(+ 2 3 4 5 6 7 8 9 1)

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
 "git-integration.el")

(config/load-all
 "modes"
 "haste-mode.el")
(put 'dired-find-alternate-file 'disabled nil)
