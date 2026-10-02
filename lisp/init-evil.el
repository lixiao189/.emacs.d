;;; init-evil.el --- Vim emulation -*- lexical-binding: t; -*-

(use-package evil
  :demand t
  :init
  ;; evil 1.15 reads this, but Emacs 31 no longer defines it.
  (defvar evil-mode-buffers nil)
  (setq evil-want-C-u-scroll t
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
  ;; Keep the selection after < and > (LazyVim).
  (evil-define-key 'visual 'global
    ">" #'my/visual-shift-right
    "<" #'my/visual-shift-left))

(defun my/visual-shift-right ()
  "Indent the selection and keep it selected."
  (interactive)
  (call-interactively #'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/visual-shift-left ()
  "Dedent the selection and keep it selected."
  (interactive)
  (call-interactively #'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(use-package evil-collection
  :after evil
  :demand t
  :init
  (setq evil-collection-setup-minibuffer nil
        evil-collection-key-blacklist '("SPC" "M-SPC"))
  :config
  ;; Only the modes we use; faster than setting up all of them.
  (evil-collection-init
   '(dired help info xref compile grep ibuffer flymake eglot eldoc
     magit magit-todos diff-hl package-menu custom ediff eshell
     corfu vertico consult embark which-key calendar)))

;; gsa/gsd/gsr like LazyVim's mini.surround; ys/cs/ds/S still work.
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

;; gc/gcc comment, gco/gcO add a comment below/above (LazyVim).
(use-package evil-commentary
  :after evil
  :demand t
  :config
  (evil-commentary-mode 1)
  (evil-define-key 'normal 'global
    "gco" #'my/comment-below
    "gcO" #'my/comment-above))

(defun my/comment-below ()
  "Start a comment on a new line below."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (insert comment-start " ")
  (evil-insert-state))

(defun my/comment-above ()
  "Start a comment on a new line above."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert comment-start " ")
  (indent-according-to-mode)
  (evil-insert-state))

;; Tree-sitter text objects like LazyVim's mini.ai:
;; f function, c class, a argument, o block/conditional/loop; ]f/[f jump.
(use-package evil-textobj-tree-sitter
  :after evil
  :demand t
  :config
  ;; `evil-textobj-tree-sitter-get-textobj' is a macro, so no loop here.
  (let ((outer evil-outer-text-objects-map)
        (inner evil-inner-text-objects-map))
    (define-key outer "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
    (define-key inner "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
    (define-key outer "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
    (define-key inner "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
    (define-key outer "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
    (define-key inner "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
    (define-key outer "o" (evil-textobj-tree-sitter-get-textobj
                            ("conditional.outer" "loop.outer" "block.outer")))
    (define-key inner "o" (evil-textobj-tree-sitter-get-textobj
                            ("conditional.inner" "loop.inner" "block.inner"))))
  (evil-define-key 'normal 'global
    "]f" #'my/next-function
    "[f" #'my/previous-function))

(defun my/next-function ()
  (interactive)
  (evil-textobj-tree-sitter-goto-textobj "function.outer"))

(defun my/previous-function ()
  (interactive)
  (evil-textobj-tree-sitter-goto-textobj "function.outer" t))

;; s: jump anywhere, like flash.nvim.
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
