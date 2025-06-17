;; the prefix is config/

(setq package-enable-at-startup nil)

(defmacro config/load-all (root &rest names)
  (unless (stringp root)
    (error "Root is exepected to be a string"))
  (let ((root-path (expand-file-name root user-emacs-directory)))
    (dolist (name names)
      (unless (stringp name)
        (error (format "`%s` isn't a string. got `%s`"
                       name (type-of name))))
      (load-file (expand-file-name name root-path)))))

(config/load-all
 "lisp"
 "emacs-options.el"
 "bootstrap.el"
 "modals.el"
 "keybindings.el"
 "movement.el"
 "completions.el"
 "dired-conf.el"
 "visuals.el"
 "ide.el"
 "git-integration.el")

