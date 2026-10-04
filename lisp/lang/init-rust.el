;;; init-rust.el --- Rust -*- lexical-binding: t; -*-

(use-package rust-mode
  :init (setq rust-indent-offset 4))

(my/lang-setup '(rust-mode))

;; rust-analyzer is eglot's default server for Rust.
(with-eval-after-load 'eglot
  (my/eglot-workspace-config
   :rust-analyzer '(:check (:command "clippy")
                    :cargo (:allFeatures t)
                    :completion (:callable (:snippets "add_parentheses"))
                    :inlayHints (:chainingHints (:enable t)
                                 :parameterHints (:enable t)
                                 :typeHints (:enable t)))))

;; Plain rustfmt defaults to edition 2015, which rejects modern syntax.
(with-eval-after-load 'apheleia
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2024")))

(provide 'init-rust)
;;; init-rust.el ends here
