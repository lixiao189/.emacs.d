;;; init-rust.el --- Rust -*- lexical-binding: t; -*-

(add-to-list 'treesit-language-source-alist
             '(rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3"))

;; There is no built-in non-tree-sitter rust-mode, so map the extension directly.
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(setq rust-ts-mode-indent-offset 4)

(my/lang-setup '(rust-ts-mode) t)

;; rust-analyzer is eglot's built-in default server.
(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                (plist-put
                 (default-value 'eglot-workspace-configuration)
                 :rust-analyzer '(:check (:command "clippy")
                                  :cargo (:allFeatures t)
                                  :completion (:callable (:snippets "add_parentheses"))
                                  :inlayHints (:chainingHints (:enable t)
                                               :parameterHints (:enable t)
                                               :typeHints (:enable t))))))

;; Bare rustfmt assumes edition 2015 and rejects async/let-else; pass a modern one.
(with-eval-after-load 'apheleia
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2024")))

(provide 'init-rust)
;;; init-rust.el ends here
