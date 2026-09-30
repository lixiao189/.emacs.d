;;; init-ui.el --- Look and feel -*- lexical-binding: t; -*-

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-tokyo-night t))

;; Font: first one that exists.
(defun my/setup-font (&optional frame)
  (when (display-graphic-p frame)
    (when-let* ((font (seq-find (lambda (f) (find-font (font-spec :name f)))
                                '("JetBrainsMono Nerd Font Mono" "JetBrains Mono"
                                  "Fira Code" "SF Mono" "Menlo"))))
      (set-face-attribute 'default frame :family font :height 140))))
(my/setup-font)
(add-hook 'after-make-frame-functions #'my/setup-font)

;; Scrolling: smooth trackpad, LazyVim-like scrolloff.
(setq scroll-margin 4
      scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode 1)

;; Line numbers (relative, like LazyVim) + current line highlight in code buffers.
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)
(dolist (h '(prog-mode-hook conf-mode-hook))
  (add-hook h #'display-line-numbers-mode)
  (add-hook h #'hl-line-mode))
(column-number-mode 1)
(setq-default truncate-lines t)

;; Mode line: vim/lualine-like. [STATE] branch file ... diag lsp mode enc pos.
(setq evil-mode-line-format nil)        ; we render the state ourselves

(defface my/ml-normal '((t :inherit mode-line-emphasis :inverse-video t)) "Normal state.")
(defface my/ml-insert '((t :inherit success :inverse-video t)) "Insert state.")
(defface my/ml-visual '((t :inherit warning :inverse-video t)) "Visual state.")
(defface my/ml-replace '((t :inherit error :inverse-video t)) "Replace state.")
(defface my/ml-emacs '((t :inherit font-lock-keyword-face :inverse-video t)) "Emacs state.")

(defun my/ml-state ()
  (let* ((st (and (boundp 'evil-state) evil-state))
         (spec (pcase st
                 ('normal   '(" NORMAL " my/ml-normal))
                 ('insert   '(" INSERT " my/ml-insert))
                 ('visual   '(" VISUAL " my/ml-visual))
                 ('replace  '(" REPLACE " my/ml-replace))
                 ('operator '(" O-PENDING " my/ml-normal))
                 ('motion   '(" MOTION " my/ml-normal))
                 (_         '(" EMACS " my/ml-emacs)))))
    (propertize (car spec) 'face (cadr spec))))

(defun my/ml-vc ()
  (when (and vc-mode buffer-file-name)
    (let* ((br (replace-regexp-in-string "\\`[ ]*[A-Za-z]+[-:@!?]" "" (substring-no-properties vc-mode)))
           (st (vc-state buffer-file-name))
           (face (pcase st
                   ('edited 'warning) ('added 'success) ('conflict 'error)
                   ('unregistered 'shadow) (_ 'success))))
      (concat " " (propertize (concat "\ue0a0 " br) 'face face)
              (when (memq st '(edited added conflict))
                (propertize " +" 'face face))))))

(defun my/ml-diag ()
  (when (bound-and-true-p flymake-mode)
    (let* ((e 0) (w 0) (n 0))
      (dolist (d (flymake-diagnostics))
        (pcase (flymake--severity (flymake-diagnostic-type d))
          ((pred (<= 2)) (cl-incf e))
          (1 (cl-incf w))
          (_ (cl-incf n))))
      (concat (propertize (format " E:%d" e) 'face (if (> e 0) 'error 'shadow))
              (propertize (format " W:%d" w) 'face (if (> w 0) 'warning 'shadow))))))

(defun my/ml-lsp ()
  (when (bound-and-true-p eglot--managed-mode)
    (let ((srv (and (fboundp 'eglot-current-server) (eglot-current-server))))
      (if srv
          (propertize (format " LSP:%s" (or (ignore-errors (eglot-project-nickname srv)) "on"))
                      'face 'success
                      'help-echo (format "eglot: %s" (ignore-errors (eglot--server-info srv))))
        (propertize " LSP…" 'face 'warning)))))

(setq-default
 mode-line-format
 '("%e"
   (:eval (my/ml-state))
   (:eval (my/ml-vc))
   " "
   (:propertize "%b" face mode-line-buffer-id)
   (:eval (cond (buffer-read-only (propertize " RO" 'face 'warning))
                ((buffer-modified-p) (propertize " [+]" 'face 'error))))
   mode-line-format-right-align
   (:eval (my/ml-diag))
   (:eval (my/ml-lsp))
   (:eval (when (bound-and-true-p defining-kbd-macro) (propertize " REC" 'face 'error)))
   " " (:propertize mode-name face bold)
   " " (:eval (let ((c (coding-system-eol-type buffer-file-coding-system)))
                (concat (upcase (replace-regexp-in-string
                                 "-.*" "" (symbol-name (coding-system-base buffer-file-coding-system))))
                        (pcase c (0 "/LF") (1 "/CRLF") (2 "/CR") (_ "")))))
   "  %l:%c  %p "))

;; Tabs (LazyVim <leader><tab>): only show the bar when there is >1 tab.
(setq tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-new-tab-choice "*scratch*")

(setq window-divider-default-right-width 1
      window-divider-default-places 'right-only)
(add-hook 'emacs-startup-hook #'window-divider-mode)

;; which-key is built in.
(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0.05
      which-key-sort-order #'which-key-key-order-alpha
      which-key-add-column-padding 2
      which-key-min-display-lines 4)
(add-hook 'emacs-startup-hook #'which-key-mode)

(defun my/open-config (&rest _)
  "Open the config folder in dired."
  (interactive)
  (dired user-emacs-directory))

;; Welcome screen (LazyVim/alpha-like). Nerd icons are optional; skipped if unavailable.
(use-package dashboard
  :demand t
  :bind (:map dashboard-mode-map ("c" . my/open-config))
  :init
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer)
        dashboard-navigator-buttons
        '((("" "Config (c)" "Open the config folder" my/open-config))))
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-display-icons-p nil
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-projects-backend 'project-el
        dashboard-items '((recents . 8) (projects . 5))
        dashboard-item-shortcuts '((recents . "r") (projects . "p"))
        dashboard-set-footer nil
        dashboard-banner-logo-title "Emacs")
  ;; Also show it for `emacsclient -c` and new frames.
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'dashboard-mode 'motion)))

(provide 'init-ui)
;;; init-ui.el ends here
