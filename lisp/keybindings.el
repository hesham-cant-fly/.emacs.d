(use-package general
	:ensure t
	:config
	(general-create-definer config/leader-def
		:keymaps 'override
		:states '(normal visual emacs)
		:prefix "SPC")
	(general-create-definer config/local-leader-def
		:keymaps 'override
		:states '(normal visual emacs)
		:prefix "SPC m")

	;; Search
	(config/leader-def
		"s" '(:ignore t :wk "Search"))

	;; Files
	(config/leader-def
		:states 'normal
		"f"   '(:ignore t :wk "Files")
		"f f" '(find-file :wk "Find File")
		"f r" '(recentf   :wk "Recent Files")
		"f C" '((lambda ()
							(interactive)
							(find-file user-emacs-directory))
						          :wk "Open dired in user config")
		"f c" '((lambda ()
							(interactive)
							(find-file (expand-file-name "init.el" user-emacs-directory)))
						          :wk "Open init.el file"))

	;; Dired
	(config/leader-def
		:states 'normal
		"d"   '(:ignore t             :wk "Dired")
		"d d" '(dired-jump            :wk "Open Dired"))

	;; Buffers
	(config/leader-def
		:states 'normal
		"b"   '(:ignore t              :wk "Buffers")
		"b i" '(ibuffer                :wk "Open IBuffer")
		"b b" '(persp-switch-to-buffer :wk "Find a Buffer")
		"b k" '(kill-current-buffer    :wk "Kill This Buffer")
		"b r" '(revert-buffer          :wk "Reload buffer"))

	;; Project
	(config/leader-def
		:states 'normal
		"SPC" '(project-find-file :wk "Find File")

		"p"   '(:ignore t                       :wk "Project")
		"p p" '(project-switch-project          :wk "Find Project")
		"p a" '(project-remember-projects-under :wk "Add Known Project")
		"p r" '(project-forget-project          :wk "Forgot Project")
		"p R" '(project-compile                 :wk "Run Project")
		"p q" '(project-kill-buffers            :wk "Quit Project"))

	;; Magit
	(config/leader-def
		:states 'normal
		"g"   '(:ignore t             :wk "Git")
		"g g" '(magit                 :wk "Open Magit"))

	;; Compilation
	(config/leader-def
		:states 'normal
		"c"   '(:ignore t             :wk "Compilation")
		"c c" '(compile               :wk "Open Compilation Mode"))

	;; Toggle Stuff
	(config/leader-def
		:states 'normal
		"t"   '(:ignore t             :wk "Toggle")
		"t t" '(toggle-truncate-lines :wk "Toggle Truncate Lines")
		"t d" '(rainbow-delimiters-mode :wk "Toggle rainbow-delimiter-mode"))

	;; Help
	(config/leader-def
		:states 'normal
		"h"   '(:ignore t             :wk "Help")
		"h v" '(describe-variable     :wk "Describe Variable")
		"h f" '(describe-function     :wk "Describe Function")
		"h k" '(describe-key          :wk "Describe Key")))

(elpaca-wait) ;; Wait's for general to be initialized
