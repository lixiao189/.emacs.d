;;; init-keys.el --- LazyVim keymaps (https://www.lazyvim.org/keymaps) -*- lexical-binding: t; -*-

(use-package general
  :demand t
  :after evil
  :config
  (general-evil-setup)
  (general-create-definer my/leader
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC"
    :non-normal-prefix "M-SPC"))

;;;; Helpers

;; "root" = project root (LazyVim "root dir"), "cwd" = `default-directory'.
(defun my/root ()
  "Project root, else `default-directory'."
  (or (when-let* ((project (project-current))) (project-root project))
      default-directory))

(defun my/symbol-at-point ()
  (thing-at-point 'symbol t))

;; Find and grep
(defun my/find-file-root () (interactive) (consult-fd (my/root)))
(defun my/find-file-cwd  () (interactive) (consult-fd default-directory))
(defun my/find-config    () (interactive) (consult-fd user-emacs-directory))
(defun my/grep-root      () (interactive) (consult-ripgrep (my/root)))
(defun my/grep-cwd       () (interactive) (consult-ripgrep default-directory))
(defun my/grep-word-root () (interactive) (consult-ripgrep (my/root) (my/symbol-at-point)))
(defun my/grep-word-cwd  () (interactive) (consult-ripgrep default-directory (my/symbol-at-point)))

;; Files, explorer, terminal
(defun my/new-file ()
  (interactive)
  (find-file (read-file-name "New file: " default-directory)))

(defun my/explorer (dir)
  "Toggle the treemacs sidebar with DIR as its only project.
Replacing the workspace (like Doom) stops a persisted project from
shadowing DIR."
  (require 'treemacs)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (let ((dir (treemacs-canonical-path dir)))
      (treemacs--show-single-project dir (file-name-nondirectory dir)))))

(defun my/explorer-root () (interactive) (my/explorer (my/root)))
(defun my/explorer-cwd  () (interactive) (my/explorer default-directory))

(defun my/terminal-root ()
  (interactive)
  (let ((default-directory (my/root)))
    (eshell 'new)))

;; Buffers and windows
(defun my/kill-other-buffers ()
  "Kill every file buffer except the current one."
  (interactive)
  (mapc #'kill-buffer
        (delq (current-buffer) (seq-filter #'buffer-file-name (buffer-list)))))

(defun my/toggle-maximize ()
  (interactive)
  (if (one-window-p) (winner-undo) (delete-other-windows)))

(defun my/window-taller   () (interactive) (enlarge-window 2))
(defun my/window-shorter  () (interactive) (shrink-window 2))
(defun my/window-narrower () (interactive) (shrink-window-horizontally 2))
(defun my/window-wider    () (interactive) (enlarge-window-horizontally 2))

(defun my/clear-search-and-escape ()
  "Clear search highlight and return to normal state."
  (interactive)
  (evil-ex-nohighlight)
  (evil-force-normal-state))

;; Diagnostics and code
(defun my/line-diagnostics ()
  (interactive)
  (if-let* ((diags (flymake-diagnostics (line-beginning-position) (line-end-position))))
      (message "%s" (mapconcat #'flymake-diagnostic-text diags "\n"))
    (message "No diagnostics on this line")))

(defun my/goto-diag (direction severity)
  "Return a command jumping to the next/previous diagnostic of SEVERITY."
  (lambda ()
    (interactive)
    (if (eq direction 'next)
        (flymake-goto-next-error 1 severity t)
      (flymake-goto-prev-error 1 severity t))))

(defun my/code-action-source ()
  (interactive)
  (eglot-code-actions nil nil "source" t))

;; UI toggles
(defun my/toggle-relative-numbers ()
  (interactive)
  (setq display-line-numbers-type
        (if (eq display-line-numbers-type 'relative) t 'relative))
  (display-line-numbers-mode -1)
  (display-line-numbers-mode 1))

(defun my/toggle-autoformat ()
  (interactive)
  (apheleia-mode 'toggle)
  (message "Auto format: %s" (if apheleia-mode "on" "off")))

;;;; Non-leader keys

;; Let windmove enter treemacs, which is marked `no-other-window'.
(setq windmove-allow-all-windows t)

(general-def :states '(normal motion)
  ;; windows
  "C-h" #'evil-window-left
  "C-j" #'evil-window-down
  "C-k" #'evil-window-up
  "C-l" #'evil-window-right
  "C-<up>"    #'my/window-taller
  "C-<down>"  #'my/window-shorter
  "C-<left>"  #'my/window-narrower
  "C-<right>" #'my/window-wider
  ;; buffers
  "H"  #'previous-buffer
  "L"  #'next-buffer
  "[b" #'previous-buffer
  "]b" #'next-buffer
  ;; LSP / goto
  "gd" #'xref-find-definitions
  "gr" #'xref-find-references
  "gI" #'eglot-find-implementation
  "gy" #'eglot-find-typeDefinition
  "gD" #'xref-find-definitions-other-window
  "K"  #'eldoc-doc-buffer
  "gK" #'eldoc-doc-buffer
  ;; diagnostics
  "]d" (my/goto-diag 'next nil)
  "[d" (my/goto-diag 'prev nil)
  "]e" (my/goto-diag 'next :error)
  "[e" (my/goto-diag 'prev :error)
  "]w" (my/goto-diag 'next :warning)
  "[w" (my/goto-diag 'prev :warning)
  "]q" #'next-error
  "[q" #'previous-error
  ;; git hunks
  "]h" #'diff-hl-next-hunk
  "[h" #'diff-hl-previous-hunk
  "<escape>" #'my/clear-search-and-escape)

;; treemacs has its own evil state; let C-h/C-l leave the sidebar.
(with-eval-after-load 'treemacs-evil
  (general-def :keymaps 'evil-treemacs-state-map
    "C-h" #'evil-window-left
    "C-l" #'evil-window-right))

(general-def :states '(normal visual insert emacs)
  "C-s" #'save-buffer
  "M-j" #'move-text-down
  "M-k" #'move-text-up
  "C-." #'embark-act)

;; ESC quits the minibuffer straight away.
(keymap-set minibuffer-local-map "<escape>" #'abort-minibuffers)

;;;; Leader keys
(my/leader
  "SPC" '(my/find-file-root :which-key "Find Files (Root Dir)")
  "," '(consult-buffer :which-key "Switch Buffer")
  "/" '(my/grep-root :which-key "Grep (Root Dir)")
  ":" '(consult-complex-command :which-key "Command History")
  "`" '(evil-switch-to-windows-last-buffer :which-key "Switch to Other Buffer")
  "e" '(my/explorer-root :which-key "Explorer (Root Dir)")
  "E" '(my/explorer-cwd :which-key "Explorer (cwd)")
  "K" '(man :which-key "Keywordprg")
  "l" '(package-list-packages :which-key "Packages")
  "-" '(evil-window-split :which-key "Split Window Below")
  "|" '(evil-window-vsplit :which-key "Split Window Right")

  ;; <tab> tabs
  "<tab>" '(:ignore t :which-key "tabs")
  "<tab><tab>" '(tab-new :which-key "New Tab")
  "<tab>d" '(tab-close :which-key "Close Tab")
  "<tab>o" '(tab-close-other :which-key "Close Other Tabs")
  "<tab>]" '(tab-next :which-key "Next Tab")
  "<tab>[" '(tab-previous :which-key "Previous Tab")
  "<tab>f" '(tab-first :which-key "First Tab")
  "<tab>l" '(tab-last :which-key "Last Tab")

  ;; b buffer
  "b" '(:ignore t :which-key "buffer")
  "bb" '(evil-switch-to-windows-last-buffer :which-key "Switch to Other Buffer")
  "bd" '(kill-current-buffer :which-key "Delete Buffer")
  "bD" '(kill-buffer-and-window :which-key "Delete Buffer and Window")
  "bo" '(my/kill-other-buffers :which-key "Delete Other Buffers")
  "bl" '(consult-buffer :which-key "List Buffers")

  ;; c code
  "c" '(:ignore t :which-key "code")
  "ca" '(eglot-code-actions :which-key "Code Action")
  "cA" '(my/code-action-source :which-key "Source Action")
  "cc" '(eglot-code-actions :which-key "Code Action (line)")
  "cd" '(my/line-diagnostics :which-key "Line Diagnostics")
  "cf" '(my/format :which-key "Format")
  "cl" '(eglot-list-connections :which-key "Lsp Info")
  "cm" '(eglot-shutdown :which-key "Stop Lsp")
  "co" '(eglot-code-action-organize-imports :which-key "Organize Imports")
  "cr" '(eglot-rename :which-key "Rename")
  "cR" '(eglot-reconnect :which-key "Restart Lsp")
  "cs" '(consult-imenu :which-key "Symbols")

  ;; f file/find
  "f" '(:ignore t :which-key "file/find")
  "fb" '(consult-buffer :which-key "Buffers")
  "fc" '(my/find-config :which-key "Find Config File")
  "ff" '(my/find-file-root :which-key "Find Files (Root Dir)")
  "fF" '(my/find-file-cwd :which-key "Find Files (cwd)")
  "fg" '(project-find-file :which-key "Find Files (git)")
  "fn" '(my/new-file :which-key "New File")
  "fp" '(project-switch-project :which-key "Projects")
  "fr" '(consult-recent-file :which-key "Recent")
  "ft" '(my/terminal-root :which-key "Terminal (Root Dir)")
  "fe" '(my/explorer-root :which-key "Explorer (Root Dir)")
  "fE" '(my/explorer-cwd :which-key "Explorer (cwd)")

  ;; g git
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "Status")
  "gs" '(magit-status :which-key "Status")
  "gb" '(magit-blame-addition :which-key "Git Blame")
  "gd" '(magit-diff-unstaged :which-key "Diff")
  "gl" '(magit-log-current :which-key "Log")
  "gf" '(magit-log-buffer-file :which-key "Current File History")
  "gc" '(magit-commit :which-key "Commit")
  "gp" '(magit-push :which-key "Push")
  "gP" '(magit-pull :which-key "Pull")
  "gh" '(:ignore t :which-key "hunks")
  "ghs" '(diff-hl-stage-current-hunk :which-key "Stage Hunk")
  "ghr" '(diff-hl-revert-hunk :which-key "Reset Hunk")
  "ghp" '(diff-hl-show-hunk :which-key "Preview Hunk")

  ;; q quit/session
  "q" '(:ignore t :which-key "quit/session")
  "qq" '(save-buffers-kill-terminal :which-key "Quit All")

  ;; s search
  "s" '(:ignore t :which-key "search")
  "s\"" '(consult-register :which-key "Registers")
  "sb" '(consult-line :which-key "Buffer")
  "sc" '(consult-complex-command :which-key "Command History")
  "sC" '(execute-extended-command :which-key "Commands")
  "sd" '(consult-flymake :which-key "Diagnostics")
  "sg" '(my/grep-root :which-key "Grep (Root Dir)")
  "sG" '(my/grep-cwd :which-key "Grep (cwd)")
  "sh" '(describe-symbol :which-key "Help Pages")
  "sj" '(evil-show-jumps :which-key "Jumplist")
  "sk" '(describe-bindings :which-key "Key Maps")
  "sm" '(consult-mark :which-key "Jump to Mark")
  "sM" '(consult-man :which-key "Man Pages")
  "sr" '(query-replace-regexp :which-key "Search and Replace")
  "sR" '(vertico-repeat :which-key "Resume")
  "ss" '(consult-imenu :which-key "Goto Symbol")
  "sS" '(xref-find-apropos :which-key "Goto Symbol (Workspace)")
  "sw" '(my/grep-word-root :which-key "Word (Root Dir)")
  "sW" '(my/grep-word-cwd :which-key "Word (cwd)")

  ;; u ui toggles
  "u" '(:ignore t :which-key "ui")
  "uC" '(consult-theme :which-key "Colorscheme")
  "ud" '(flymake-mode :which-key "Toggle Diagnostics")
  "uf" '(my/toggle-autoformat :which-key "Toggle Auto Format")
  "uh" '(eglot-inlay-hints-mode :which-key "Toggle Inlay Hints")
  "ui" '(describe-char :which-key "Inspect Pos")
  "uI" '(treesit-explore-mode :which-key "Inspect Tree")
  "ul" '(display-line-numbers-mode :which-key "Toggle Line Numbers")
  "uL" '(my/toggle-relative-numbers :which-key "Toggle Relative Number")
  "ur" '(evil-ex-nohighlight :which-key "Clear Search Highlight")
  "us" '(flyspell-mode :which-key "Toggle Spelling")
  "uw" '(visual-line-mode :which-key "Toggle Wrap")

  ;; w windows
  "w" '(:ignore t :which-key "windows")
  "ww" '(evil-window-next :which-key "Other Window")
  "wd" '(evil-window-delete :which-key "Delete Window")
  "w-" '(evil-window-split :which-key "Split Window Below")
  "w|" '(evil-window-vsplit :which-key "Split Window Right")
  "wh" '(evil-window-left :which-key "Go to Left Window")
  "wj" '(evil-window-down :which-key "Go to Lower Window")
  "wk" '(evil-window-up :which-key "Go to Upper Window")
  "wl" '(evil-window-right :which-key "Go to Right Window")
  "wo" '(delete-other-windows :which-key "Delete Other Windows")
  "wm" '(my/toggle-maximize :which-key "Maximize Toggle")
  "w=" '(balance-windows :which-key "Equalize Windows")
  "wu" '(winner-undo :which-key "Undo Window Layout")

  ;; x diagnostics/quickfix
  "x" '(:ignore t :which-key "diagnostics/quickfix")
  "xx" '(flymake-show-buffer-diagnostics :which-key "Diagnostics (Buffer)")
  "xX" '(flymake-show-project-diagnostics :which-key "Diagnostics (Project)")
  "xq" '(consult-compile-error :which-key "Quickfix List")
  "xl" '(flymake-show-buffer-diagnostics :which-key "Location List"))

;; Markdown only.
(my/leader
  :keymaps 'markdown-mode-map
  "cp" '(my/markdown-preview :which-key "Markdown Preview"))

(provide 'init-keys)
;;; init-keys.el ends here
