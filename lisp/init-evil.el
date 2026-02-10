;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-u-scroll t)
  :hook (after-init . evil-mode)
  ;; Don't quit Emacs on `:q'.
  ;;
  ;; Rebind `f'/`s' to mimic `evil-snipe'.
  :bind (:map evil-motion-state-map
         ("f" . evil-avy-goto-char-in-line)
         :map evil-normal-state-map
         ("s" . evil-avy-goto-char-timer))
  :config
  ;; Silence line out of range error.
  (shut-up! #'evil-indent)
  :custom
  ;; undo will never freeze my Emacs
  (evil-undo-system 'undo-redo)
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  ;; Enable completing-read for : ex command Emacs command names (Vertico UI).
  (evil-ex-complete-emacs-commands t)
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; when `visual-line-mode' enabled, exchange j/k with gj/gk
  (evil-respect-visual-line-mode t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-want-C-g-bindings t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-symbol-word-search t))

(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :hook (after-init . evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :hook (evil-mode . evil-collection-init)
  :bind (([remap evil-show-marks] . evil-collection-consult-mark)
         ([remap evil-show-jumps] . evil-collection-consult-jump-list))
  :config
  ;; Make `evil-collection-consult-mark' and `evil-collection-consult-jump-list'
  ;; immediately available.
  (evil-collection-require 'consult)
  :custom
  (evil-collection-setup-debugger-keys nil)
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-unimpaired-want-repeat-mode-integration t))

;; evil leader map
(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup t)

(general-create-definer init/leader
  :states '(normal visual motion)
  :keymaps 'override
  :prefix "SPC")


  (init/leader
    "SPC" '(keyboard-escape-quit :which-key "quit")
    "S-SPC" '(lazy-highlight-cleanup :which-key "clear hl")

    "'" '(vertico-repeat :which-key "resume")
    ";" '(avy-resume :which-key "avy resume")

    "e" '(find-file :which-key "find file")

    "f" '(:ignore t :which-key "files")
    "ff" '(consult-fd :which-key "find file")
    "fb" '(switch-to-buffer :which-key "switch buffer")
    "fs" '(consult-buffer :which-key "search buffer")
    "fw" '(consult-ripgrep :which-key "rg")

    "b" '(:ignore t :which-key "bufmark")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bc" '(+kill-other-buffers :which-key "kill other")
    "bC" '(+kill-all-buffers :which-key "kill all")
    "c" '(kill-current-buffer :which-key "kill buffer")

    "l" '(:ignore t :which-key "language")
    "la" '(eglot-code-actions :which-key "code actions")
    "ld" '(flymake-show-diagnostic :which-key "diagnostic")
    "lD" '(flymake-show-buffer-diagnostics :which-key "buffer diags")
    "lf" '(+format-buffer :which-key "format")
    "lG" '(xref-find-apropos :which-key "workspace symbols")
    "lr" '(eglot-rename :which-key "rename")
    "ls" '(consult-imenu :which-key "buffer symbols")

    "t" '(:ignore t :which-key "tab")
    "tc" '(tab-bar-close-tab :which-key "close tab")
    "tC" '(tab-bar-close-group-tabs :which-key "close group")
    "tg" '(tab-bar-change-tab-group :which-key "group")
    "ti" '(tab-switcher :which-key "switcher")
    "tn" '(tab-bar-new-tab :which-key "new tab")
    "to" '(tab-bar-close-other-tabs :which-key "close others")
    "tt" '(tab-bar-switch-to-tab :which-key "switch tab")
    "t'" '(tab-bar-switch-to-recent-tab :which-key "recent tab")
    "tr" '(tab-bar-rename-tab :which-key "rename tab")

    "p" '(projectile-command-map :which-key "project")

    "g" '(:ignore t :which-key "git")
    "gr" '(diff-hl-revert-hunk :which-key "revert hunk")
    "gR" '(vc-revert :which-key "vc revert")
    "gs" '(diff-hl-stage-current-hunk :which-key "stage hunk")
    "gp" '(diff-hl-show-hunk :which-key "show hunk")

    "a" '(:ignore t :which-key "app")
    "aa" '(org-agenda :which-key "agenda")
    "ac" '(calendar :which-key "calendar")
    "ag" '(gnus :which-key "gnus")
    "ai" '(rcirc :which-key "irc")

    "o" '(:ignore t :which-key "open")
    "oc" '(org-capture :which-key "capture")
    "ol" '(org-store-link :which-key "store link")
    "ot" '(ansi-term :which-key "ansi term")
    "oe" '(eshell :which-key "eshell")
    "os" '(shell :which-key "shell"))

  (general-define-key
   :states '(normal)
   :keymaps 'override
   "]d" #'flymake-goto-next-error
   "[d" #'flymake-goto-prev-error
   "]g" #'diff-hl-next-hunk
   "[g" #'diff-hl-previous-hunk
   "]b" #'next-buffer
   "[b" #'previous-buffer)

  ;; Window navigation on Ctrl+h/j/k/l in normal state only.
  (require 'windmove)
  (general-define-key
   :states '(normal)
   :keymaps 'override
   "C-h" #'windmove-left
   "C-j" #'windmove-down
   "C-k" #'windmove-up
   "C-l" #'windmove-right)
  )

(provide 'init-evil)
;;; init-evil.el ends here
