;;; init-git.el --- Magit, git gutter -*- lexical-binding: t; -*-

;; Point/scroll saved across a refresh triggered from a visual selection.
(defvar-local my/magit--refresh-state nil)

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
        magit-save-repository-buffers 'dontask)
  ;; Keep point and scroll position when staging/unstaging a visual
  ;; selection, instead of jumping back to the section start (Doom).
  (add-hook 'magit-pre-refresh-hook
            (defun my/magit-save-window-state-h ()
              (when (use-region-p)
                (setq my/magit--refresh-state
                      (list (current-buffer) (region-beginning) (window-start))))))
  (add-hook 'magit-post-refresh-hook
            (defun my/magit-restore-window-state-h ()
              (pcase-let ((`(,buf ,pt ,start) my/magit--refresh-state))
                (when (and buf (eq buf (current-buffer)))
                  (goto-char pt)
                  (set-window-start nil start t)
                  (kill-local-variable 'my/magit--refresh-state)))))
  ;; Clickable URLs in the process buffer.
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  ;; Start the commit message in insert state when it is empty, otherwise
  ;; normal state (amend/reword).
  (add-hook 'git-commit-setup-hook
            (defun my/git-commit-insert-state-maybe-h ()
              (when (and (bound-and-true-p evil-local-mode)
                         (not (evil-emacs-state-p))
                         (bobp) (eolp))
                (evil-insert-state)))))

(defun my/magit-quit (&optional kill-buffer)
  "Bury the current magit buffer; kill all of the repo's magit buffers
when no other magit window for it is left (Doom `+magit/quit')."
  (interactive "P")
  (let ((topdir (magit-toplevel)))
    (funcall magit-bury-buffer-function kill-buffer)
    (unless (seq-some (lambda (win)
                        (with-selected-window win
                          (and (derived-mode-p 'magit-mode)
                               (equal magit--default-directory topdir))))
                      (window-list))
      (my/magit-quit-all))))

(defun my/magit-quit-all ()
  "Kill all magit buffers for the current repository."
  (interactive)
  (dolist (buf (magit-mode-get-buffers))
    (when (buffer-live-p buf)
      (let ((proc (get-buffer-process buf)))
        (unless (and proc (process-live-p proc))
          (kill-buffer buf))))))

;; Doom's evil tweaks on top of evil-collection-magit. Folds go under z
;; (za/zo/zc/z1-z4, stash moves to Z) like in code buffers.
(setq evil-collection-magit-use-z-for-folds t
      evil-collection-magit-section-use-z-for-folds t)

(defun my/magit-evil-setup-h (mode &rest _)
  "Extra vim-isms for magit, run after evil-collection sets it up."
  (when (eq mode 'magit)
    ;; q is enough; ESC is too easy to hit by accident in the status buffer.
    (evil-define-key* 'normal magit-status-mode-map [escape] nil)
    (evil-define-key* '(normal visual) magit-mode-map
      "q"  #'my/magit-quit
      "Q"  #'my/magit-quit-all
      "*"  #'magit-worktree
      "zt" #'evil-scroll-line-to-top
      "zz" #'evil-scroll-line-to-center
      "zb" #'evil-scroll-line-to-bottom
      "g=" #'magit-diff-default-context)
    ;; Don't open a process buffer from inside the process buffer.
    (evil-define-key* '(normal visual) magit-process-mode-map "`" #'ignore)
    ;; TAB toggles the section at point.
    (dolist (map (list magit-status-mode-map magit-stash-mode-map
                       magit-revision-mode-map magit-process-mode-map
                       magit-diff-mode-map))
      (evil-define-key* 'normal map [tab] #'magit-section-toggle))
    ;; 1-4 / M-1..M-4 / 0 mask count prefixes; z1-z4 cover them.
    (dolist (key '("1" "2" "3" "4" "0" "M-1" "M-2" "M-3" "M-4"))
      (define-key magit-section-mode-map (kbd key) nil t))
    ;; Move commits with gj/gk too (M-j/M-k still work).
    (with-eval-after-load 'git-rebase
      (evil-define-key* evil-collection-magit-state git-rebase-mode-map
        "gj" #'git-rebase-move-line-down
        "gk" #'git-rebase-move-line-up))))
(add-hook 'evil-collection-setup-hook #'my/magit-evil-setup-h)

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
