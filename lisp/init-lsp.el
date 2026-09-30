;;; init-lsp.el --- Eglot, tree-sitter, C++ / Go / Python -*- lexical-binding: t; -*-

;;;; Tree-sitter (built in) -------------------------------------------------------
;; Grammars: M-x my/treesit-install-grammars (needs a C compiler; macOS has cc).
(setq treesit-language-source-alist
      '((c      "https://github.com/tree-sitter/tree-sitter-c" "v0.23.4")
        (cpp    "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
        (go     "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
        (gomod  "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
        (bash   "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
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

(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.work\\'" . go-work-ts-mode))

;;;; Language settings -------------------------------------------------------------
(setq c-ts-mode-indent-offset 4
      c-ts-mode-indent-style 'k&r
      go-ts-mode-indent-offset 4
      python-indent-offset 4
      python-indent-guess-indent-offset nil)

(add-hook 'go-ts-mode-hook (lambda () (setq tab-width 4 indent-tabs-mode t)))
(add-hook 'go-mode-hook    (lambda () (setq tab-width 4 indent-tabs-mode t)))

;;;; Eglot (built in) ---------------------------------------------------------------
(use-package eglot
  :ensure nil
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode
          go-mode go-ts-mode go-mod-ts-mode
          python-mode python-ts-mode) . eglot-ensure)
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
  ;; --- servers ---
  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ,(eglot-alternatives
                     '(("clangd" "--background-index" "--clang-tidy"
                        "--header-insertion=iwyu" "--completion-style=detailed"
                        "--function-arg-placeholders=false" "-j=4"
                        "--fallback-style=llvm")
                       "ccls"))))
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 . ,(eglot-alternatives
                     '(("basedpyright-langserver" "--stdio")
                       ("pyright-langserver" "--stdio")
                       "pylsp"
                       ("ruff" "server")))))
  ;; go: gopls is the built-in default.

  (setq-default eglot-workspace-configuration
                '(:gopls (:staticcheck t
                          :usePlaceholders :json-false
                          :completeUnimported t
                          :analyses (:unusedparams t :shadow t)
                          :hints (:assignVariableTypes t :compositeLiteralFields t
                                  :compositeLiteralTypes t :constantValues t
                                  :functionTypeParameters t :parameterNames t
                                  :rangeVariableTypes t))
                  :basedpyright.analysis (:typeCheckingMode "standard")))

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

;;;; Formatting (LazyVim: format on save via conform.nvim) ---------------------------
(use-package apheleia
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode
          go-mode go-ts-mode python-mode python-ts-mode) . apheleia-mode)
  :config
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  (dolist (m '(go-mode go-ts-mode))
    (setf (alist-get m apheleia-mode-alist)
          (if (executable-find "goimports") 'goimports 'gofmt)))
  (dolist (m '(python-mode python-ts-mode))
    (setf (alist-get m apheleia-mode-alist) '(ruff-isort ruff))))

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
