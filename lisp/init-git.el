;;; init-git.el --- Magit, git gutter -*- lexical-binding: t; -*-

;; Git manager (LazyVim lazygit). Keys are under <leader>g in init-keys.el.
(use-package magit
  :init
  (setq transient-levels-file (concat my/var-dir "transient-levels.el")
        transient-values-file (concat my/var-dir "transient-values.el"))
  :config
  ;; Status takes the whole frame, `q' restores the previous layout.
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration
        magit-diff-refine-hunk t
        magit-save-repository-buffers 'dontask))

;; Git gutter (LazyVim gitsigns).
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (conf-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq diff-hl-draw-borders nil))

(provide 'init-git)
;;; init-git.el ends here
