;; Cask loading

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "/usr/local/Cellar/cask/0.7.2_1/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Org config loading
(require 'org)
(setq org-confirm-babel-evaluate nil)
(setq vc-follow-symlinks nil)
(org-babel-load-file "~/.emacs.d/readme.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (htmlize web-mode scala-mode pallet pager org-plus-contrib on-screen ocodo-svg-modelines multiple-cursors js2-mode helm-projectile helm-ag expand-region editorconfig csharp-mode company-tern color-theme-sanityinc-tomorrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
