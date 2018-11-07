;; Cask loading

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'cask "/usr/local/opt/cask/cask.el")
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
 '(ansi-color-faces-vector
	 [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
	 (vector "#003f8e" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#ffffff"))
 '(beacon-color "#ff9da4")
 '(custom-safe-themes
	 (quote
		("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(fci-rule-color "#003f8e")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(org-agenda-files nil)
 '(package-selected-packages
	 (quote
		(yaml-mode htmlize web-mode scala-mode pallet pager org-plus-contrib on-screen ocodo-svg-modelines multiple-cursors js2-mode helm-projectile helm-ag expand-region editorconfig csharp-mode company-tern color-theme-sanityinc-tomorrow)))
 '(safe-local-variable-values
	 (quote
		((intero-targets "helloworld:lib" "helloworld:test:helloworld-test"))))
 '(tab-width 2)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#ff9da4")
		 (40 . "#ffc58f")
		 (60 . "#ffeead")
		 (80 . "#d1f1a9")
		 (100 . "#99ffff")
		 (120 . "#bbdaff")
		 (140 . "#ebbbff")
		 (160 . "#ff9da4")
		 (180 . "#ffc58f")
		 (200 . "#ffeead")
		 (220 . "#d1f1a9")
		 (240 . "#99ffff")
		 (260 . "#bbdaff")
		 (280 . "#ebbbff")
		 (300 . "#ff9da4")
		 (320 . "#ffc58f")
		 (340 . "#ffeead")
		 (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-css-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
