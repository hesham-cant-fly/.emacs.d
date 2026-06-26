(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/skeletons/")
(defun as-c-header-guard-name (s)
  "adds _H_ at the end of s"
  (concat s "_H_"))

(defun dih-name--- ()
  ""
  (-> (buffer-file-name)
	  file-name-nondirectory
	  file-name-sans-extension
	  upcase
	  as-c-header-guard-name))

(setq-default auto-insert-alist '())
(define-auto-insert
  '("\\.h\\'" . "C / C++ header")
  '(nil
	"/** Created in: " (format-time-string "%d/%M/%Y %H:%m") "\n"
	"  *\n"
	"  */\n"
	"#ifndef " (dih-name---) "\n"
	"#define " (dih-name---) "\n"
	"\n"
	"\n"
	"\n"
	"#endif /* !" (dih-name---) " */\n"))

(define-auto-insert
  '("\\.hpp\\'" . "C / C++ header")
  '(nil
	"/** Created in: " (format-time-string "%d/%M/%Y %H:%m") "\n"
	"  *\n"
	"  */\n"
	"#ifndef " (dih-name---) "\n"
	"#define " (dih-name---) "\n"
	"\n"
	"\n"
	"\n"
	"#endif /* !" (dih-name---) " */\n"))

