;;; init-lsp.el --- Shared tree-sitter, eglot and formatting setup -*- lexical-binding: t; -*-
;; Per-language settings live in lisp/lang/init-<lang>.el.

;;;; Tree-sitter

;; Shared grammars; each init-<lang>.el adds its own with `my/add-grammars'.
(setq treesit-language-source-alist
      '((bash  "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
        (json  "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
        (yaml  "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0")
        (toml  "https://github.com/tree-sitter-grammars/tree-sitter-toml" "v0.7.0")
        (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.5.0")))

(setq treesit-font-lock-level 4
      treesit-auto-install-grammar 'ask)
(setopt treesit-enabled-modes t)   ; use *-ts-mode when its grammar is installed

(defun my/add-grammars (&rest grammars)
  "Add GRAMMARS, each (LANG URL REVISION), to `treesit-language-source-alist'."
  (dolist (grammar grammars)
    (add-to-list 'treesit-language-source-alist grammar)))

(defun my/treesit-install-grammars ()
  "Install every grammar in `treesit-language-source-alist' that is missing."
  (interactive)
  (require 'treesit)
  (dolist (entry treesit-language-source-alist)
    (unless (treesit-language-available-p (car entry))
      (treesit-install-language-grammar (car entry)))))

;;;; Eglot

(defun my/eglot-setup-h ()
  "Per-buffer setup for eglot-managed buffers."
  ;; Inlay hints start off (LazyVim default); toggle with SPC u h.
  (eglot-inlay-hints-mode -1)
  ;; Refetch LSP candidates on each keystroke, then fall back to files/dabbrev.
  (setq-local completion-at-point-functions
              (list (cape-capf-noninterruptible
                     (cape-capf-buster #'eglot-completion-at-point))
                    #'cape-file #'cape-dabbrev)))

(use-package eglot
  :ensure nil
  :init
  (setq eglot-autoshutdown t
        eglot-sync-connect nil          ; don't block the UI on server startup
        eglot-connect-timeout 60
        eglot-send-changes-idle-time 0.3
        eglot-extend-to-xref t
        eglot-events-buffer-config '(:size 0 :format short) ; no LSP log
        eglot-report-progress nil
        eglot-code-action-indications '(eldoc-hint)
        eldoc-echo-area-use-multiline-p 3
        flymake-no-changes-timeout 0.5
        ;; clangd's on-type formatting fights electric-pair on RET.
        eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  :config
  (add-hook 'eglot-managed-mode-hook #'my/eglot-setup-h))

;; Optional: `cargo install emacs-lsp-booster' for faster LSP.
(use-package eglot-booster
  :if (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  :after eglot
  :config (eglot-booster-mode 1))

(defun my/lang-setup (modes)
  "Start eglot automatically in each of MODES."
  (dolist (mode modes)
    (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure)))

(defun my/eglot-workspace-config (section settings)
  "Set SECTION (a keyword like :gopls) of `eglot-workspace-configuration'."
  (setq-default eglot-workspace-configuration
                (plist-put (default-value 'eglot-workspace-configuration)
                           section settings)))

;;;; Formatting

;; No format on save. Format with SPC c f; toggle per buffer with SPC u f.
;; Languages register their formatters in their own files.
(use-package apheleia
  :commands apheleia-mode)

(defun my/format ()
  "Format the buffer with apheleia, falling back to the LSP server."
  (interactive)
  (require 'apheleia)
  (let ((formatters (apheleia--get-formatters)))
    (cond (formatters (apheleia-format-buffer formatters))
          ((and (fboundp 'eglot-managed-p) (eglot-managed-p)) (eglot-format-buffer))
          (t (user-error "No formatter available")))))

(provide 'init-lsp)
;;; init-lsp.el ends here
