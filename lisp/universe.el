(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '(
                  ;; ("https://planet.emacslife.com/atom.xml" emacs editor tools)
                  ;; ("https://www.reddit.com/r/emacs.rss" emacs editor tools)
                  ;; ("https://www.reddit.com/r/neovim.rss" vim editor tools)
                  ("https://news.ycombinator.com/rss" programming)
                  ("https://www.reddit.com/r/ProgrammingLanguages.rss" programming)
                  ("https://www.reddit.com/r/osdev.rss" programming)))
  (url-queue-timeout 30)
  :config
  :general
  (config/leader-def
    "o e" '(elfeed :wk "Elfeed")))
