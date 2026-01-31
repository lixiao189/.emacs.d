;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(use-package evil
  :straight t
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
  :straight t
  :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary
  :straight t
  :hook (after-init . evil-commentary-mode))

(use-package evil-collection
  :straight t
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
(use-package evil
  :straight nil
  :config
  (with-no-warnings
    ;; We use "SPC" as the leader key, "SPC m" as the localleader key. Due to the
    ;; limitation of `evil-set-leader', we can't easily set localleader key with
    ;;
    ;; ``` elisp
    ;; (evil-set-leader 'normal (kbd "SPC m") :localleader)
    ;; ```
    ;;
    ;; An error is prompted:
    ;;
    ;; ``` elisp
    ;; (error "Key sequence SPC m starts with non-prefix key SPC")
    ;; ```
    ;;
    ;; If you know how to fix that, let me know. Thanks.
    (evil-set-leader 'normal (kbd "SPC"))

    (defun define-leader-key (state map localleader &rest bindings)
      "Define leader key in MAP when STATE, a wrapper for
`evil-define-key*'. All BINDINGS are prefixed with \"<leader>\"
if LOCALLEADER is nil, otherwise \"<localleader>\"."
      (cl-assert (cl-evenp (length bindings)))
      (let ((prefix (if localleader "<localleader>" "<leader>"))
            wk-replacements)
        (while bindings
          (let ((key (pop bindings))
                (def (pop bindings)))
            (when (symbolp def)
              (evil-define-key* state map (kbd (concat prefix key)) def))
            ;; Save which-key (key . replacement).
            (pcase def
              (`(:wk ,replacement)
               (push (cons (concat "SPC " key) replacement) wk-replacements)))))
        ;; which-key integration.
        ;; XXX: replacement for localleader NOT supported.
        (cl-loop for (key . replacement) in wk-replacements
                 unless localleader
                 do (which-key-add-key-based-replacements key replacement))))

    (define-leader-key 'normal 'global nil
      ;; SPC, quit minibuffer.
      "SPC" 'keyboard-escape-quit

      ;; Clear highlights
      "S-SPC" 'lazy-highlight-cleanup

      ;; Resume
      "'" 'vertico-repeat
      ";" 'avy-resume

      "e" 'find-file

      ;; file
      "f"  '(:wk "files")
      "ff" 'consult-fd
      "fb" 'switch-to-buffer
      "fs" 'consult-buffer
      "fw" 'consult-ripgrep

      ;; buffer & bookmark
      "b" '(:wk "bufmark")
      "bb" 'switch-to-buffer
      "bc" '+kill-other-buffers
      "bC" '+kill-all-buffers
      "c" 'kill-current-buffer

      ;; language
      "l" '(:wk "language")
      "la" 'eglot-code-actions
      "ld" 'flymake-show-diagnostic
      "lD" 'flymake-show-buffer-diagnostics
      "lf" '+format-buffer
      "lG" 'xref-find-apropos
      "lr" 'eglot-rename
      "ls" 'consult-imenu

      ;; tab
      "t" '(:wk "tab")
      "tc" 'tab-bar-close-tab
      "tC" 'tab-bar-close-group-tabs
      "tg" 'tab-bar-change-tab-group
      "ti" 'tab-switcher
      "tn" 'tab-bar-new-tab
      "to" 'tab-bar-close-other-tabs
      "tt" 'tab-bar-switch-to-tab
      "t'" 'tab-bar-switch-to-recent-tab
      "tr" 'tab-bar-rename-tab

      ;; project
      "p" 'projectile-command-map

      ;; git
      "g" '(:wk "git")
      "gr" 'diff-hl-revert-hunk
      "gR" 'vc-revert
      "gs" 'diff-hl-stage-current-hunk
      "gp" 'diff-hl-show-hunk

      ;; app
      "a" '(:wk "app")
      "aa" 'org-agenda
      "ac" 'calendar
      "ag" 'gnus
      "ai" 'rcirc

      ;; open
      "o" '(:wk "open")
      "oc" 'org-capture
      "ol" 'org-store-link
      "ot" 'ansi-term
      "oe" 'eshell
      "os" 'shell)

    (evil-define-key* 'normal 'global (kbd "]d") #'flymake-goto-next-error)
    (evil-define-key* 'normal 'global (kbd "[d") #'flymake-goto-prev-error)
    (evil-define-key* 'normal 'global (kbd "]g") #'diff-hl-next-hunk)
    (evil-define-key* 'normal 'global (kbd "[g") #'diff-hl-previous-hunk)
    (evil-define-key* 'normal 'global (kbd "]b") #'next-buffer)
    (evil-define-key* 'normal 'global (kbd "[b") #'previous-buffer)

    ;; Window navigation on Ctrl+h/j/k/l in normal/visual states only.
    (require 'windmove)
    (evil-define-key* 'normal 'global (kbd "C-h") #'windmove-left)
    (evil-define-key* 'normal 'global (kbd "C-j") #'windmove-down)
    (evil-define-key* 'normal 'global (kbd "C-k") #'windmove-up)
    (evil-define-key* 'normal 'global (kbd "C-l") #'windmove-right)

    (with-eval-after-load 'elisp-mode
      (dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
        (define-leader-key 'normal keymap :localleader
          "i" 'info-lookup-symbol

          ;; eval
          "eb" 'eval-buffer
          "ed" 'eval-defun
          "ee" 'eval-last-sexp
          "el" 'load-library

          ;; goto
          "gf" 'find-function
          "gv" 'find-variable
          "gl" 'find-library)))))

(provide 'init-evil)
;;; init-evil.el ends here
