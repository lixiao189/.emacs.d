;;; init-ui.el --- Look and feel -*- lexical-binding: t; -*-

;; Same pairing as Ghostty: light = Catppuccin Latte, dark = Catppuccin Mocha.
(use-package catppuccin-theme
  :demand t
  :config
  (setq catppuccin-flavor 'mocha
        catppuccin-italic-comments t
        catppuccin-italic-variables t)
  (defun my/apply-appearance (&optional appearance)
    "Load the Catppuccin flavor that matches the macOS APPEARANCE."
    (setq catppuccin-flavor
          (if (eq (or appearance ns-system-appearance) 'light) 'latte 'mocha))
    (catppuccin-reload)
    (dolist (frame (frame-list))
      (when (display-graphic-p frame)
        (set-frame-parameter frame 'ns-appearance
                             (if (eq catppuccin-flavor 'latte) 'light 'dark))
        (set-frame-parameter frame 'ns-transparent-titlebar t))))
  (my/apply-appearance)
  (when (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions #'my/apply-appearance)))

;;;; Fonts

;; The first installed font in each list wins.
;; The first installed font wins. Ghostty uses Maple Mono NF CN at 14pt;
;; that family already covers CJK, so no separate han font or rescaling.
(defvar my/mono-fonts '("Maple Mono NF CN"
                        "JetBrainsMono Nerd Font Mono" "JetBrains Mono" "Fira Code"
                        "SF Mono" "Menlo" "Consolas" "DejaVu Sans Mono"))

(defun my/first-font (families frame)
  "Return the first of FAMILIES available on FRAME."
  (seq-find (lambda (f) (find-font (font-spec :family f) frame)) families))

(defun my/setup-font (&optional frame)
  "Set the default font on FRAME. Maple Mono NF CN covers CJK too."
  (when (display-graphic-p frame)
    (when-let* ((font (my/first-font my/mono-fonts frame)))
      ;; :height is tenths of a point; Ghostty font-size = 14.
      (set-face-attribute 'default frame
                          :family font :height 140 :weight 'regular
                          :slant 'normal)
      (set-face-attribute 'fixed-pitch frame :family font :height 140)
      (set-face-attribute 'variable-pitch frame :family font :height 140)
      ;; Ghostty adjust-cell-height = 15%.
      (set-frame-parameter frame 'line-spacing 0.15)
      (dolist (charset '(han cjk-misc kana bopomofo))
        (set-fontset-font t charset (font-spec :family font) frame)))))
(my/setup-font)
(add-hook 'after-make-frame-functions #'my/setup-font)

;;;; Editing view

;; Smooth trackpad scrolling, LazyVim-like scrolloff.
(setq scroll-margin 4
      scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode 1)

;; Relative line numbers and current-line highlight in code buffers.
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)
(dolist (h '(prog-mode-hook conf-mode-hook))
  (add-hook h #'display-line-numbers-mode)
  (add-hook h #'hl-line-mode))
(column-number-mode 1)
(setq-default truncate-lines t)

;;;; Mode line

;; lualine-like: STATE branch file ... diagnostics lsp mode encoding position.
(setq evil-mode-line-format nil)        ; the state is drawn by `my/ml-state'

(defface my/ml-normal '((t :inherit mode-line-emphasis :inverse-video t)) "Normal state.")
(defface my/ml-insert '((t :inherit success :inverse-video t)) "Insert state.")
(defface my/ml-visual '((t :inherit warning :inverse-video t)) "Visual state.")
(defface my/ml-replace '((t :inherit error :inverse-video t)) "Replace state.")
(defface my/ml-emacs '((t :inherit font-lock-keyword-face :inverse-video t)) "Emacs state.")

(defun my/ml-state ()
  (let* ((spec (pcase (bound-and-true-p evil-state)
                 ('normal   '(" NORMAL " my/ml-normal))
                 ('insert   '(" INSERT " my/ml-insert))
                 ('visual   '(" VISUAL " my/ml-visual))
                 ('replace  '(" REPLACE " my/ml-replace))
                 ('operator '(" O-PENDING " my/ml-normal))
                 ('motion   '(" MOTION " my/ml-normal))
                 (_         '(" EMACS " my/ml-emacs)))))
    (propertize (car spec) 'face (cadr spec))))

(defun my/ml-vc ()
  "Branch name, colored by the file's VC state."
  (when (and vc-mode buffer-file-name)
    (let* ((branch (replace-regexp-in-string
                    "\\`[ ]*[A-Za-z]+[-:@!?]" "" (substring-no-properties vc-mode)))
           (state (vc-state buffer-file-name))
           (face (pcase state
                   ('edited 'warning)
                   ('conflict 'error)
                   ('unregistered 'shadow)
                   (_ 'success))))
      (concat " " (propertize (concat "\ue0a0 " branch) 'face face)
              (when (memq state '(edited added conflict))
                (propertize " +" 'face face))))))

(defun my/ml-diag ()
  "Flymake error and warning counts."
  (when (bound-and-true-p flymake-mode)
    (let ((errors 0) (warnings 0))
      (dolist (diag (flymake-diagnostics))
        (pcase (flymake--severity (flymake-diagnostic-type diag))
          ((pred (<= 2)) (cl-incf errors))
          (1 (cl-incf warnings))))
      (concat (propertize (format " E:%d" errors) 'face (if (> errors 0) 'error 'shadow))
              (propertize (format " W:%d" warnings) 'face (if (> warnings 0) 'warning 'shadow))))))

(defun my/ml-lsp ()
  "LSP status: project name when connected, \"LSP…\" while connecting."
  (when (bound-and-true-p eglot--managed-mode)
    (if-let* ((server (and (fboundp 'eglot-current-server) (eglot-current-server))))
        (propertize (format " LSP:%s" (or (ignore-errors (eglot-project-nickname server)) "on"))
                    'face 'success
                    'help-echo (format "eglot: %s" (ignore-errors (eglot--server-info server))))
      (propertize " LSP…" 'face 'warning))))

(defun my/ml-buffer-status ()
  (cond (buffer-read-only (propertize " RO" 'face 'warning))
        ((buffer-modified-p) (propertize " [+]" 'face 'error))))

(defun my/ml-macro ()
  (when defining-kbd-macro (propertize " REC" 'face 'error)))

(defun my/ml-encoding ()
  "Coding system and line endings, e.g. UTF-8/LF."
  (concat (upcase (symbol-name (coding-system-base buffer-file-coding-system)))
          (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "/LF") (1 "/CRLF") (2 "/CR") (_ ""))))

(setq-default
 mode-line-format
 '("%e"
   (:eval (my/ml-state))
   (:eval (my/ml-vc))
   " "
   (:propertize "%b" face mode-line-buffer-id)
   (:eval (my/ml-buffer-status))
   mode-line-format-right-align
   (:eval (my/ml-diag))
   (:eval (my/ml-lsp))
   (:eval (my/ml-macro))
   " " (:propertize mode-name face bold)
   " " (:eval (my/ml-encoding))
   "  %l:%c  %p "))

;;;; Tabs, windows, which-key

;; Show the tab bar only with more than one tab.
(setq tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-new-tab-choice "*scratch*")

(setq window-divider-default-right-width 1
      window-divider-default-places 'right-only)
(add-hook 'emacs-startup-hook #'window-divider-mode)

(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0.05
      which-key-sort-order #'which-key-key-order-alpha
      which-key-add-column-padding 2
      which-key-min-display-lines 4)
(add-hook 'emacs-startup-hook #'which-key-mode)

;;;; File tree (SPC e / SPC E)

(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :init
  (setq treemacs-persist-file (concat my/var-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat my/var-dir "treemacs-persist-at-last-error")
        treemacs-width 32
        treemacs-is-never-other-window t
        treemacs-no-png-images t        ; text markers, no icons needed
        treemacs-follow-after-init t)
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 'simple))

(use-package treemacs-evil
  :demand t
  :after (treemacs evil))

(defun my/open-config (&rest _)
  "Open the config folder in dired."
  (interactive)
  (dired user-emacs-directory))

;;;; Dashboard

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
        '((("" "Config (c)" "Open the config folder" my/open-config)))
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-display-icons-p nil
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-projects-backend 'project-el
        dashboard-items '((recents . 8) (bookmarks . 5) (projects . 5))
        dashboard-item-shortcuts '((recents . "r") (bookmarks . "m") (projects . "p"))
        dashboard-set-footer nil
        dashboard-banner-logo-title "Emacs")
  ;; Show it for `emacsclient -c' too, but not when files are given on the
  ;; command line (that would split the frame with an empty dashboard).
  (when (or (daemonp) (< (length command-line-args) 2))
    (setq initial-buffer-choice
          (lambda ()
            (dashboard-insert-startupify-lists)
            (get-buffer-create dashboard-buffer-name))))
  :config
  (dashboard-setup-startup-hook)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'dashboard-mode 'motion)))

(provide 'init-ui)
;;; init-ui.el ends here
