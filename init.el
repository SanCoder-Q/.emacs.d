;; Cask loading
(require 'cask "/usr/local/Cellar/cask/0.7.2_1/cask.el")
(cask-initialize)


;; Org config loading
(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")
