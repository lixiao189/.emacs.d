;;; init-evil.el --- Vim emulation -*- lexical-binding: t; -*-

(use-package evil
  :demand t
  :init
  ;; Emacs 31 globalized modes no longer define `evil-mode-buffers', which
  ;; evil 1.15 reads in `evil-initializing-p'.
  (defvar evil-mode-buffers nil)
  (setq evil-want-integration t
        evil-want-keybinding nil          ; evil-collection handles it
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil            ; keep TAB as TAB
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-redo
        evil-respect-visual-line-mode t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-search-module 'evil-search
        evil-ex-search-vim-style-regexp t
        evil-symbol-word-search t
        evil-shift-width 4
        evil-shift-round t
        evil-move-beyond-eol nil
        evil-esc-delay 0
        evil-kill-on-visual-paste nil)
  :config
  (evil-mode 1)
  ;; Keep the visual selection when indenting (LazyVim: < and >).
  (evil-define-key 'visual 'global
    (kbd ">") (lambda () (interactive) (call-interactively #'evil-shift-right) (evil-normal-state) (evil-visual-restore))
    (kbd "<") (lambda () (interactive) (call-interactively #'evil-shift-left) (evil-normal-state) (evil-visual-restore))))

(use-package evil-collection
  :after evil
  :demand t
  :init
  (setq evil-collection-setup-minibuffer nil
        evil-collection-key-blacklist '("SPC" "M-SPC"))
  :config
  ;; Only the modes we use: cheaper startup than (evil-collection-init).
  (evil-collection-init
   '(dired help info xref compile grep ibuffer flymake eglot eldoc
     magit magit-todos diff-hl package-menu custom ediff eshell
     corfu vertico consult embark which-key calendar)))

;; gsa / gsd / gsr = mini.surround (LazyVim); ys/cs/ds/S keep working too.
(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'normal 'global
    "gsa" #'evil-surround-edit
    "gsd" #'evil-surround-delete
    "gsr" #'evil-surround-change)
  (evil-define-key 'visual 'global "gsa" #'evil-surround-region))

;; gc / gcc = comment operator (+ gco / gcO like LazyVim).
(use-package evil-commentary
  :after evil
  :demand t
  :config
  (evil-commentary-mode 1)
  (evil-define-key 'normal 'global
    "gco" (lambda () (interactive)
            (end-of-line) (newline-and-indent) (insert comment-start " ")
            (evil-insert-state))
    "gcO" (lambda () (interactive)
            (beginning-of-line) (open-line 1) (insert comment-start " ")
            (indent-according-to-mode) (evil-insert-state))))

;; `s` = flash.nvim-style jump.
(use-package avy
  :commands (avy-goto-char-timer)
  :init
  (setq avy-timeout-seconds 0.25
        avy-all-windows t
        avy-background t)
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global "s" #'avy-goto-char-timer)))

(use-package move-text :commands (move-text-up move-text-down))

(provide 'init-evil)
;;; init-evil.el ends here
