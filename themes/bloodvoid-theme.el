(deftheme bloodvoid
  "ported from https://github.com/cococry/dots/blob/main/nvim/bloodvoid.lua")

(let ((class '((class color) (min-colors 89)))
      ;; Color Palette Definitions
      (bg-black       "#000000")
      (bg-dark        "#151515")
      (bg-one         "#181818")
      (bg-two         "#202020")
      (bg-three       "#303030")
      
      (fg-main        "#d8d8d8")
      (fg-muted       "#b8b8b8")
      (fg-dark-grey   "#505050")
      (fg-comment     "#707070")
      (fg-delimiter   "#8a8a8a")
      
      (selection-bg   "#c8c8c8")
      (search-bg      "#b8b8b8")
      (search-inc-bg  "#e0e0e0")

      ;; Specific Token Accent Hues
      (syntax-string  "#415237")
      (syntax-escape  "#516346")
      (syntax-num     "#aaaaaa")
      (syntax-bool    "#c6c6c6")
      (syntax-fn      "#635a46")
      (syntax-keyword "#99883f")
      (syntax-macro   "#8f6a5a"))

  (custom-theme-set-faces
   'bloodvoid

   ;; --- Standard Core UI ---
   `(default ((,class (:foreground ,fg-main :background ,bg-black))))
   `(fringe ((,class (:foreground ,fg-dark-grey :background ,bg-black))))
   `(cursor ((,class (:background ,fg-main))))
   `(region ((,class (:foreground ,bg-black :background ,selection-bg))))
   `(secondary-selection ((,class (:background ,bg-three))))
   `(line-number ((,class (:foreground ,fg-dark-grey :background ,bg-black))))
   `(line-number-current-line ((,class (:foreground ,fg-main :background ,bg-one :weight bold))))
   `(hl-line ((,class (:background ,bg-one))))
   `(vertical-border ((,class (:foreground ,fg-comment))))
   `(minibuffer-prompt ((,class (:foreground ,fg-main :weight bold))))

   ;; --- Search & Selection Overlays ---
   `(isearch ((,class (:foreground ,bg-black :background ,search-inc-bg :weight bold))))
   `(lazy-highlight ((,class (:foreground ,bg-black :background ,search-bg))))
   `(match ((,class (:foreground ,bg-black :background ,search-bg :weight bold))))

   ;; --- Mode Line (Statusline) ---
   `(mode-line ((,class (:foreground ,fg-main :background ,bg-two :box nil))))
   `(mode-line-inactive ((,class (:foreground ,fg-comment :background ,bg-one :box nil))))

   ;; --- Standard Syntax Highlighting (Font Lock) ---
   `(font-lock-comment-face ((,class (:foreground ,fg-comment :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-comment :slant italic))))
   `(font-lock-string-face ((,class (:foreground ,syntax-string))))
   `(font-lock-doc-face ((,class (:foreground ,syntax-string :slant italic))))
   `(font-lock-number-face ((,class (:foreground ,syntax-num))))
   `(font-lock-negation-char-face ((,class (:foreground ,fg-main :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,syntax-keyword :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,syntax-fn :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg-main))))
   `(font-lock-type-face ((,class (:foreground ,fg-muted))))
   `(font-lock-constant-face ((,class (:foreground ,fg-muted))))
   `(font-lock-builtin-face ((,class (:foreground ,fg-muted :weight bold))))
   `(font-lock-warning-face ((,class (:foreground ,bg-black :background ,selection-bg :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,syntax-macro :weight bold))))

   ;; --- Tree-sitter Specifics (For modern Emacs 29+ ts-modes) ---
   `(treesit-face-comment ((,class (:foreground ,fg-comment :slant italic))))
   `(treesit-face-string ((,class (:foreground ,syntax-string))))
   `(treesit-face-number ((,class (:foreground ,syntax-num))))
   `(treesit-face-keyword ((,class (:foreground ,syntax-keyword :weight bold))))
   `(treesit-face-function ((,class (:foreground ,syntax-fn :weight bold))))
   `(treesit-face-type ((,class (:foreground ,fg-muted))))
   `(treesit-face-property ((,class (:foreground ,fg-muted))))
   `(treesit-face-operator ((,class (:foreground ,fg-muted))))
   `(treesit-face-punctuation ((,class (:foreground ,fg-delimiter))))

   ;; --- Dired / File Managers ---
   `(dired-directory ((,class (:foreground ,fg-muted :weight bold))))
   `(dired-symlink ((,class (:foreground ,fg-muted :underline t))))
   `(dired-header ((,class (:foreground ,fg-main :weight bold))))

   ;; --- Popups, Vertico, and Corfu ---
   `(corfu-default ((,class (:foreground ,fg-main :background ,bg-two))))
   `(corfu-current ((,class (:foreground ,bg-black :background ,selection-bg :weight bold))))
   `(vertico-current ((,class (:foreground ,bg-black :background ,selection-bg :weight bold))))
   `(completions-common-part ((,class (:foreground "#f0f0f0" :weight bold))))

   ;; --- UI Cleanups ---
   `(shadow ((,class (:foreground ,fg-comment))))
   `(error ((,class (:foreground ,bg-black :background ,selection-bg :weight bold))))
   `(warning ((,class (:foreground ,bg-black :background ,search-bg :weight bold))))
   `(success ((,class (:foreground ,fg-main :weight bold))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'bloodvoid)
