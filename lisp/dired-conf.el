(use-package dired
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  :general
  (:states 'normal :keymaps 'dired-mode-map
		   "h" '(dired-up-directory :wk "Up Directory")
		   "l" '(dired-find-file :wk "Open File"))
  (config/leader-def
	:states 'normal
	"d d" '(dired-posframe :wk "Open Dired on a posframe")
	"d p" '((lambda ()
			  (dired-posframe (expand-file-name "~/Documents/Projects/")))
			:wk "Open Project Directory on a posframe")))

(use-package all-the-icons-dired
  :ensure t
  
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package sudo-edit
  :ensure t
  :general
  (config/leader-def
	:states 'normal
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit           :wk "Sudo this file")))

(defvar posframe-dired--buffer-name nil
  "The buffer itself")
(defvar posframe-dired--frame nil
  "The current frame")

(define-derived-mode posframe-dired-mode dired-mode "Dired Posframe"
  "Major Mode for `dired-mode'."
  (dired-build-subdir-alist)
  (general-def 'normal 'posframe-dired-mode-map
    "h" #'posframe-dired-up-directory
    "l" #'posframe-dired-find-file
	"RET" #'posframe-dired-find-file
    "q" #'posframe-dired-close))

(defun dired-posframe (&optional path)
  ""
  (interactive)
  (posframe-dired-close)
  (when (posframe-workable-p)
    (let* ((current-frame (selected-frame))
           (fwidth (frame-width current-frame))
           (fheight (frame-height current-frame))
           (child-fwidth (round (/ fwidth 1.3)))
           (child-fheight (round (/ fheight 1.3)))
		   (frame nil))

	  (setq posframe-dired--buffer-name (dired-noselect (or path default-directory)))

	  (with-current-buffer posframe-dired--buffer-name
		(unless (derived-mode-p 'posframe-dired-mode)
		  (posframe-dired-mode)))

	  (setq frame (posframe-show
				   posframe-dired--buffer-name
				   :cursor 'box
				   :accept-focus t
				   :poshandler #'posframe-poshandler-frame-center
				   :override-parameters '((no-other-frame . t))
				   ;; :hidehandler #'posframe-hidehandler-when-buffer-switch
				   :width child-fwidth
				   :height child-fheight
				   :internal-border-width 1
				   :internal-border-color "gray"))
	  (setq posframe-dired--frame frame)

	  (with-selected-frame frame
		(select-frame-set-input-focus frame)
		frame))))

(defun posframe-dired-close ()
  "Hide and delete the dired posframe."
  (interactive)
  (when posframe-dired--buffer-name
	(posframe-delete posframe-dired--buffer-name)
	(setq posframe-dired--buffer-name nil
		  posframe-dired--frame nil)))

(defun posframe-dired-up-directory (&optional other-window)
  "Go up a directory within the same posframe."
  (interactive "P" posframe-dired-mode)
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (dired-posframe up)))

(defun posframe-dired-find-file ()
  "Find file or open directory in the same posframe."
  (interactive nil posframe-dired-mode)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-directory-p file-name)
        (dired-posframe file-name)
      (progn
        (posframe-dired-close)
        (find-file file-name)))))
