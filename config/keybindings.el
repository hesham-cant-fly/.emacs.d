(global-set-key (kbd "C-=")            'text-scale-increase)
(global-set-key (kbd "C--")            'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>")   'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(general-define-key
 :states   '(normal visual emacs dashboard)
 "TAB"     'tab-line-switch-to-next-tab
 [backtab] 'tab-line-switch-to-prev-tab
 "S-TAB"   'tab-line-switch-to-prev-tab)

;; space key as the global leader key
(general-create-definer hesham-cant-config/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC" ;; setting leader
  :global-prefix "M-SPC") ;; Meta key + space to access leader in insert mode
;; 'g' key as the global goto key
(general-create-definer hesham-cant-config/goto-keys
  :states '(normal visual emacs)
  :prefix "g")

; Buffers
(hesham-cant-config/leader-keys
  "b"    '(:ignore t                  :wk "Buffer")
  "bb"   '(counsel-projectile-switch-to-buffer     
           :wk "Switch buffer")
  "bi"   '(ibuffer                    :wk "IBuffer")
  "bk"   '(kill-this-buffer           :wk "Kill this buffer")
  "bn"   '(next-buffer                :wk "Next buffer")
  "bp"   '(previous-buffer            :wk "Previous buffer")
  "br"   '(revert-buffer              :wk "Reload buffer"))

(hesham-cant-config/leader-keys
  "d"    '(:ignore t                  :wk "Dired")
  "dd"   '(dired                      :wk "Open Dired")
  "dj"   '(dired-jump                 :wk "Dired jump to the current")
  "dn"   '(neotree-dir                :wk "Open directory in neotree")
  "dp"   '(peep-dired                 :wk "Peep-dired"))

; Telescope-like stuff
(hesham-cant-config/leader-keys
  "f"    '(:ignore t                  :wk "Find")
  "fr"   '(counsel-recentf            :wk "Find recent files")
  "fc"   '((lambda ()
	         (interactive)
	         (find-file "~/.emacs.d/init.el"))
	       :wk "Edit emacs config")
  "ff"   '(counsel-find-file          :wk "Find File"))

; Evaluating Emacs lisp
(hesham-cant-config/leader-keys
  "e"   '(:ignore t                  :wk "EShell/Evaluate")    
  "eb"  '(eval-buffer                :wk "Evaluate elisp in buffer")
  "ed"  '(eval-defun                 :wk "Evaluate defun containing or after point")
  "ee"  '(eval-expression            :wk "Evaluate and elisp expression")
  "el"  '(eval-last-sexp             :wk "Evaluate elisp expression before point")
  "er"  '(eval-region                :wk "Evaluate elisp in region")

  ; Eshel
  "eh"  '(counsel-esh-history        :wk "Eshell history")
  "es"  '(eshell                     :wk "Eshell"))

; Help Stuff
(hesham-cant-config/leader-keys
  "h"   '(:ignore t                  :wk "Help")
  "hf"  '(describe-function          :wk "Describe function")
  "ht"  '(load-theme                 :wk "Load a theme")
  "hv"  '(describe-variable          :wk "Describe variable")
  "hrr" '(reload-init-file           :wk "Reload emacs config"))

; Toggle Stuff
(hesham-cant-config/leader-keys
  "t"   '(:ignore t                  :wk "Toggle")
  "tc"  '((lambda ()
            (interactive)
            (tab-line-close-tab))
          :wk "Close tab and it's buffer")
  "tl"  '(display-line-numbers-mode  :wk "Toggle line numbers")
  "tn"  '(neotree-toggle             :wk "Toggle neotree file viewer")
  "tt"  '(visual-line-mode           :wk "Toggle truncated lines")
  "tv"  '(vterm-toggle               :wk "Toggle vterm"))

  ; Window Stuff
  (hesham-cant-config/leader-keys
    "w"   '(:ignore t                  :wk "Windows")
    ;; Move Windows
    "wH"  '(buf-move-left              :wk "Buffer move left")
    "wJ"  '(buf-move-down              :wk "Buffer move down")
    "wK"  '(buf-move-up                :wk "Buffer move up")
    "wL"  '(buf-move-right             :wk "Buffer move right"))

  (hesham-cant-config/leader-keys
    "r"  '(:ignore                     :wk "Run")
    "rc" '(app-launcher-run-app        :wk "App launcher"))

  (hesham-cant-config/goto-keys
    "cc" '(comment-line               :wk "Comment lines")
    "l"  '(avy-goto-line              :wk "Quick line travel")
    "s"  '(avy-goto-char-timer        :wk "Avy mode")
    "r"  '(avy-resume                 :wk "Resume Avy"))
