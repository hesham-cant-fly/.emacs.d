(defun config/workspace-exist (name)
  "Returns non-nil if the work space exits"
  (if (member name (persp-names)) t nil))

(defun config/workspace-create (name)
  "Creates a workspace with a name."
  (when (config/workspace-exist name)
    (error "A workspace named '%s' already exists" name))
  (let (persp (persp-add-new name))
    (save-window-excursion
      persp)))

(defface config/face/workspace-tab-selected '((t (:inherit highlight)))
  "The face for selected tabs displayed by `config/workspace-display'"
  :group 'persp-mode)

(defface config/face/workspace-tab '((t (:inherit default)))
  "The face for selected tabs displayed by `config/workspace-display'"
  :group 'persp-mode)

(defun config/workspace-tabline (&optional names)
  ""
  (let ((names (or names (persp-names)))
        (current-name (persp-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (+ 1 i) name)
                          'face (if (equal current-name name)
                                    'config/face/workspace-tab-selected
                                  'config/face/workspace-tab)))
     "")))

(defun config/workspace-switch (name &optional create)
  "Switch to a workspace"
  (unless (config/workspace-exist name)
    (if create
        (config/workspace-create name)
      (error "%s does not exits to switch to." name)))
  (persp-switch name))

(defun config/workspace-make-id ()
  "Generates an id"
  (let (current-max
        (names (persp-names)))
    (dolist (name names (or (when current-max
                              (+ 1 current-max))
                            1))
      (when (string-match-p "^#[0-9]+$" name)
        (setq current-max (max (if current-max current-max 0)
                               (string-to-number (substring name 1))))))))

(defun config/workspace-kill (workspace)
  ""
  (unless (stringp workspace)
    (setq workspace (persp-name workspace)))
  (persp-kill workspace))

(defun +config/workspace-display ()
  "Display a list of workspaces in the echo area."
  (interactive)
  (message "%s" (config/workspace-tabline)))

(defun +config/workspace-new (&optional name)
  "Creates a new workspace and switchs to it"
  (interactive)
  (unless name
    (setq name (format "#%s" (config/workspace-make-id))))
  (persp-new name)
  (persp-switch name)
  (switch-to-buffer "*dashboard*")
  (+config/workspace-display))

(defun +config/workspace-delete (&optional name)
  ""
  (interactive)
  (unless name
    (setq name (persp-current-name)))
  (persp-kill name))

(defun config/get-fallback-buffer ()
  "Get the ballback buffer"
  (let (buffer-list-update-hook)
    (get-buffer-create *dashboard*)))

(defun config/kill-all-buffers (&optional buffer-list)
  "Kills all buffers and switch to *dashboard**"
  (if (null buffer-list)
      (message "No buffer to kill")
    (save-some-buffers)
    (delete-other-windows)
    (mapc #'kill-buffer buffer-list)
    (switch-to-buffer (config/fallback-buffer))))

(defun +config/workspace-switch-to (index)
  "Switch to a workspace at given index"
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (persp-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (let ((names (persp-names)))
    (cond ((numberp index)
           (let ((dest (nth index names)))
             (unless dest
               (error "No workspace at #%s" (+ 1 index)))
             (config/workspace-switch dest)))
          ((stringp index)
           (config/workspace-switch index))
          (t (error "Not a valid index: %s" index))))
  (+config/workspace-display))

(dotimes (i 10)
  (defalias (intern (format "+config/workspace-switch-to-%d" i))
    `(lambda () (interactive) (+config/workspace-switch-to ,i))
    (format "Switch to workspace #%d" (+ 1 i))))

(use-package perspective
  :ensure t
  :init
  (persp-mode 1)
  ;; (+config/workspace-new "main")
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :hook (ibuffer . (lambda ()
                     (persp-ibuffer-set-filter-groups)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic))))
  :general
  (config/leader-def
    "TAB TAB" '(+config/workspace-display     :wk "Display workspaces")
    "TAB n"   '(+config/workspace-new         :wk "New Workspace")
    "TAB d"   '(+config/workspace-delete      :wk "Kill Workspace")
    "TAB 1"   '(+config/workspace-switch-to-0 :wk "Goto 1st workspace")
    "TAB 2"   '(+config/workspace-switch-to-1 :wk "Goto 2nd workspace")
    "TAB 3"   '(+config/workspace-switch-to-2 :wk "Goto 3rd workspace")
    "TAB 4"   '(+config/workspace-switch-to-3 :wk "Goto 4th workspace")
    "TAB 5"   '(+config/workspace-switch-to-4 :wk "Goto 5th workspace")
    "TAB 6"   '(+config/workspace-switch-to-5 :wk "Goto 6th workspace")
    "TAB 7"   '(+config/workspace-switch-to-6 :wk "Goto 7th workspace")
    "TAB 8"   '(+config/workspace-switch-to-7 :wk "Goto 8th workspace")
    "TAB 9"   '(+config/workspace-switch-to-8 :wk "Goto 9th workspace")
    "TAB 0"   '(+config/workspace-switch-to-9 :wk "Goto 10th workspace")))
