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

;; Git gutter (LazyVim gitsigns).
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (conf-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq diff-hl-draw-borders nil))

(provide 'init-ui)
;;; init-ui.el ends here
