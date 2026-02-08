;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

;;; Commentary:
;;
;; git-messenger has been superseded by {C-x v h} (`vc-region-history')

;;; Code:

;; The awesome git client
;;
;; Explicit binding makes it load lazily although it's the default.
;; See `magit-define-global-key-bindings' for more information.
(use-package magit
  :straight t
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :config
  ;; Let leader key (SPC) work in Magit buffers.
  (define-key magit-mode-map (kbd "SPC") nil)
  (define-key magit-section-mode-map (kbd "SPC") nil)
  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-paint-whitespace nil)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: `diff-hl' depends on `vc'
(use-package vc
  :straight nil
  :custom
  (vc-follow-symlinks t)
  (vc-allow-async-revert t)
  (vc-allow-async-diff t)
  (vc-handled-backends '(Git)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :straight t
  :hook ((after-init         . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  :custom
  (diff-hl-update-async t))

;; Visual diff interface
(use-package ediff
  :straight nil
  ;; Restore window config after quitting ediff
  :hook ((ediff-before-setup . ediff-save-window-conf)
         (ediff-quit         . ediff-restore-window-conf))
  :config
  (defvar local-ediff-saved-window-conf nil)

  (defun ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))

  (defun ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))
  :custom
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; Setup gitignore mode
(use-package conf-mode
  :straight nil
  :mode (("\\.gitignore\\'"     . conf-unix-mode)
         ("\\.gitconfig\\'"     . conf-unix-mode)
         ("\\.gitattributes\\'" . conf-unix-mode)))

(provide 'init-git)

;;; init-git.el ends here
