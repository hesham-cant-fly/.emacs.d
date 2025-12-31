(setq-default config/custom-mode-line nil)
(setq-default config/default-mode-line-format mode-line-format)

(defun config/custom-mode-line ()
  (interactive)
  (if (not config/custom-mode-line)
	  (config/activate-custom-mode-line)
	(config/deactivate-custom-mode-line)))

(defun config/activate-custom-mode-line ()
  (setq config/custom-mode-line t)

  (kill-local-variable 'mode-line-format)
  (setq-default mode-line-format
				'("%e"
				  (:eval (config/get-custom-evil-mode-line-tag))
				  (:eval (config/get-custom-modeline-buffer-name))
				  (:eval (config/get-current-major-mode))
				  "%I:%c:%l"
				  )))

(defun config/deactivate-custom-mode-line ()
  (setq config/custom-mode-line nil)
  (kill-local-variable 'mode-line-format)
  (setq-default mode-line-format config/default-mode-line-format))

(defun config/get-custom-modeline-buffer-name ()
  ""
  (-> (buffer-name)
	  (propertize 'face 'bold)
	  (config/wrap-in-space)))

(defun config/get-custom-evil-mode-line-tag ()
  ""
  (-> (config/get-custom-evil-mode-line-tag-char)
	  (propertize 'face (config/get-evil-mode-line-tag-color))
	  (config/wrap-in-space)))

(defun config/get-current-major-mode ()
  ""
  (-> major-mode
	  (symbol-name)
	  (capitalize)
	  (propertize 'face 'bold)
	  (config/string-prepend "λ")
	  (config/wrap-in-space)))

(defface config/normal-mode-face
  '((t ;; :foreground "white"
	 :foreground "#1c71d8"
	 :weight bold
	 ))
  "Face for `config/custom-mode-line' normal mode indicator"
  :group 'config/custom-mode-line)
(face-spec-set
 'config/normal-mode-face
 '((t ;; :foreground "white"
	:foreground "#1c71d8"
	:weight bold
	)))

(defface config/insert-mode-face
  '((t ;; :foreground "white"
	 :foreground "#00cc66"
	 :weight bold))
  "Face for `config/custom-mode-line' insert mode indicator"
  :group 'config/custom-mode-line)
(face-spec-set
 'config/insert-mode-face
 '((t ;; :foreground "white"
	:foreground "#00cc66"
	:weight bold)))

(defface config/visual-mode-face
  '((t ;; :foreground "white"
	 :foreground "#ffff66"
	 :weight bold))
  "Face for `config/custom-mode-line' visual mode indicator"
  :group 'config/custom-mode-line)
(face-spec-set
 'config/visual-mode-face
 '((t ;; :foreground "white"
	:foreground "#ffff66"
	:weight bold)))

(defun config/get-evil-mode-line-tag-color ()
  (pcase evil-state
	('normal 'config/normal-mode-face)
	('insert 'config/insert-mode-face)
	('visual 'config/visual-mode-face)
	(else    'error)))

(defun config/get-custom-evil-mode-line-tag-char ()
  ""
  (-> (symbol-name evil-state)
	  (substring 0 1)
	  (capitalize)))

(defun config/string-prepend (STR STR2)
  ""
  (format "%s%s" STR2 STR))

(defun config/wrap-in-space (STR)
  ""
  (format " %s " STR))

(config/custom-mode-line)
