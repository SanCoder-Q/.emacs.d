(server-start)

(global-superword-mode 1) ;; 将 camel case 和以 `-` 连接的字符看作一个 word
(delete-selection-mode 1) ;; 粘贴后取消选择
(global-auto-revert-mode 1) ;; 当外部变更文档后自动更新当前文档
(electric-indent-mode t) ;; 文档自动缩进
(global-linum-mode t) ;; 显示行号

(setq-default auto-save-default nil ;; 关闭 Auto-save
              auto-save-list-file-prefix nil ;; 停止创建 auto-save-list 文件夹
              indent-tabs-mode nil ;; 使用空格代替 tab
              ring-bell-function 'ignore ;; 关闭 alarm bell
              org-cycle-emulate-tab 'white ;; Tab 键仅在空白行中为输入 tab, 其他情况均为展开或关闭 section
              make-backup-files nil) ;; 关闭自动创建备份文件

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions)) ;; 取消关闭文件时的确认

(require 'pager) ;; 使用 pager.el 代替系统的翻页, 需要在 key map 里替换

(load-theme 'sanityinc-tomorrow-blue t) ;; 修改主题
(set-frame-parameter (selected-frame) 'alpha '(95 . 80)) ;; 设置背景透明, 分别为 Emacs active 和 unactive 时的透明度
(set-background-color "#000620") ;; 加深背景颜色 (为了使背景颜色应用到所有 mode 需要修改theme文件中的背景颜色)

(set-default-font "Envy Code R") ;; 修改默认字体
(set-face-attribute 'default nil :height 200) ;; 修改默认字体缩放大小
(tool-bar-mode -1) ;; 关闭上方的工具栏
(scroll-bar-mode -1) ;; 关闭右侧的滚动条
(show-paren-mode t) ;; 高亮显示 pair 的括号

(setq split-width-threshold 1
      split-height-threshold nil) ;; 设置默认分屏为垂直分屏

(ocodo-svg-modelines-init) ;; 渲染 mode-line
(smt/set-theme 'ocodo-mesh-aqua-smt) ;; 设置 mode-line 主题

(on-screen-global-mode 1) ;; 翻页的事后加一条小黑线
(setq on-screen-highlight-method 'narrow-line)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(global-set-key (kbd "M-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-S-z") 'redo)
(global-set-key (kbd "C-M-<return>") 'set-mark-command)
(global-set-key (kbd "C-x <return>") 'pop-to-mark-command)
(global-set-key (kbd "M-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-k") 'delete-other-windows)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "C-8") 'er/expand-region)
(global-set-key (kbd "C-x b") 'helm-for-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h m") 'helm-imenu)
(global-set-key (kbd "M-s") 'helm-occur)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "<f7>") 'toggle-window-split)
(global-set-key (kbd "C-v") 'pager-page-down)
(global-set-key (kbd "M-v") 'pager-page-up)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)

(keyboard-translate ?\C-h ?\C-?)

(when (file-readable-p ".user.el") (load ".user.el"))

(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)

(projectile-global-mode)
(helm-projectile-on)

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-files t))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-ispell t))
(add-hook 'org-mode-hook 'flyspell-mode)

(require 'editorconfig)

(org-indent-mode t)

(add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC emacs-lisp \n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("j" "#+BEGIN_SRC js \n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("s" "#+BEGIN_SRC scala \n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("r" "#+BEGIN_SRC ruby \n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("h" "#+BEGIN_SRC haskell \n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("S" "#+BEGIN_SRC scheme \n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("sh" "#+BEGIN_SRC shell-script \n?\n#+END_SRC\n"))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-mode)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun y-ret-or-n-p (prompt)
  (let ((answer 'recenter)
        (padded (lambda (prompt &optional dialog)
                  (let ((l (length prompt)))
                    (concat prompt
                            (if (or (zerop l) (eq ?\s (aref prompt (1- l))))
                                "" " ")
                            (if dialog "" "(y or n) "))))))
    (cond
     (noninteractive
      (setq prompt (funcall padded prompt))
      (let ((temp-prompt prompt))
        (while (not (memq answer '(act skip)))
          (let ((str (read-string temp-prompt)))
            (cond ((member str '("" "y" "Y")) (setq answer 'act))
                  ((member str '("n" "N")) (setq answer 'skip))
                  (t (setq temp-prompt (concat "Please answer y [RET] or n.  "
                                               prompt))))))))
     ((and (display-popup-menus-p)
           last-input-event             ; not during startup
           (listp last-nonmenu-event)
           use-dialog-box)
      (setq prompt (funcall padded prompt t)
            answer (x-popup-dialog t `(,prompt ("Yes" . act) ("No" . skip)))))
     (t
      (setq prompt (funcall padded prompt))
      (while
          (let* ((scroll-actions '(recenter scroll-up scroll-down
                                            scroll-other-window scroll-other-window-down))
                 (key
                  (let ((cursor-in-echo-area t))
                    (when minibuffer-auto-raise
                      (raise-frame (window-frame (minibuffer-window))))
                    (read-key (propertize (if (memq answer scroll-actions)
                                              prompt
                                            (concat "Please answer y [RET] or n.  "
                                                    prompt))
                                          'face 'minibuffer-prompt)))))
            (setq answer (lookup-key query-replace-map (vector key) t))
            (cond
             ((memq answer '(skip act exit)) nil)
             ((eq answer 'recenter)
              (recenter) t)
             ((eq answer 'scroll-up)
              (ignore-errors (scroll-up-command)) t)
             ((eq answer 'scroll-down)
              (ignore-errors (scroll-down-command)) t)
             ((eq answer 'scroll-other-window)
              (ignore-errors (scroll-other-window)) t)
             ((eq answer 'scroll-other-window-down)
              (ignore-errors (scroll-other-window-down)) t)
             ((or (memq answer '(exit-prefix quit)) (eq key ?\e))
              (signal 'quit nil) t)
             (t t)))
        (ding)
        (discard-input))))
    (let ((ret (memq answer '(act exit))))
      (unless noninteractive
        (message "%s%c" prompt (if ret ?y ?n)))
      ret)))

(defalias 'yes-or-no-p 'y-ret-or-n-p) ;; 转换 yes/no 问题为 y/n 问题
