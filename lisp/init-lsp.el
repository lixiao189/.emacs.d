;;; init-lsp.el --- Shared tree-sitter, eglot and formatting setup -*- lexical-binding: t; -*-
;; Per-language settings live in lang/init-<lang>.el (cpp, go, python, rust).

;;;; Tree-sitter (built in) -------------------------------------------------------
;; Grammars: M-x my/treesit-install-grammars (needs a C compiler; macOS has cc).
(setq treesit-language-source-alist
      ;; C/C++, Go, Python and Rust grammars are added by their init-<lang>.el files.
      '((bash   "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
        (json   "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
        (yaml   "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0")
        (toml   "https://github.com/tree-sitter-grammars/tree-sitter-toml" "v0.7.0")
        (cmake  "https://github.com/uyha/tree-sitter-cmake" "v0.5.0")))

(setq treesit-font-lock-level 4
      treesit-auto-install-grammar 'ask)
(setopt treesit-enabled-modes t)   ; Emacs 31: remap *-mode -> *-ts-mode when grammar exists

(defun my/treesit-install-grammars ()
  "Install every grammar in `treesit-language-source-alist' that is missing."
  (interactive)
  (require 'treesit)
  (dolist (entry treesit-language-source-alist)
    (unless (treesit-language-available-p (car entry))
      (treesit-install-language-grammar (car entry)))))

;;;; Eglot (built in) ---------------------------------------------------------------
(use-package eglot
  :ensure nil
  :init
  (setq eglot-autoshutdown t
        eglot-sync-connect nil          ; never block the UI on server startup
        eglot-connect-timeout 60
        eglot-send-changes-idle-time 0.3
        eglot-extend-to-xref t
        eglot-events-buffer-config '(:size 0 :format short) ; no LSP log = less GC
        eglot-report-progress nil
        eglot-code-action-indications '(eldoc-hint)
        eldoc-echo-area-use-multiline-p 3
        flymake-no-changes-timeout 0.5)
  :config
  ;; Inlay hints exist but start off (LazyVim default); toggle with <leader>uh.
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  ;; Corfu + orderless: refetch candidates each keystroke, fall back to file/dabbrev.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-noninterruptible
                                 (cape-capf-buster #'eglot-completion-at-point))
                                #'cape-file #'cape-dabbrev)))))

;; Optional: `cargo install emacs-lsp-booster` roughly halves LSP latency.
(use-package eglot-booster
  :if (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  :after eglot
  :config (eglot-booster-mode 1))

;;;; Formatting ----------------------------------------------------------------------
;; Format on save is off by default; format manually with `SPC c f', or toggle
;; apheleia-mode per buffer with `SPC u f'. Languages register formatters in their own files.
(use-package apheleia
  :commands apheleia-mode)

(defun my/lang-setup (modes &optional eglot)
  "Enable eglot when EGLOT in each of MODES (format on save stays off)."
  (dolist (m modes)
    (let ((hook (intern (format "%s-hook" m))))
      (when eglot (add-hook hook #'eglot-ensure)))))

(defun my/format ()
  "Format buffer: apheleia when a formatter applies, else the LSP server."
  (interactive)
  (require 'apheleia)
  (let ((fmts (apheleia--get-formatters)))
    (cond (fmts (apheleia-format-buffer fmts))
          ((and (fboundp 'eglot-managed-p) (eglot-managed-p)) (eglot-format-buffer))
          (t (user-error "No formatter available")))))

(provide 'init-lsp)
;;; init-lsp.el ends here
