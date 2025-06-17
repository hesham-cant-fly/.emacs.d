;; the prefix is config/

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
 "movement.el"
 "visuals.el"
 "git-integration.el")

(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
