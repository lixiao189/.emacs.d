;;; init-python.el --- Python -*- lexical-binding: t; -*-

(add-to-list 'treesit-language-source-alist
             '(python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))

(setq python-indent-offset 4
      python-indent-guess-indent-offset nil)

(my/lang-setup '(python-mode python-ts-mode) t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 . ,(eglot-alternatives
                     '(("basedpyright-langserver" "--stdio")
                       ("pyright-langserver" "--stdio")
                       "pylsp"
                       ("ruff" "server")))))
  (setq-default eglot-workspace-configuration
                (plist-put
                 (default-value 'eglot-workspace-configuration)
                 :basedpyright.analysis '(:typeCheckingMode "standard"))))

(with-eval-after-load 'apheleia
  (dolist (m '(python-mode python-ts-mode))
    (setf (alist-get m apheleia-mode-alist) '(ruff-isort ruff))))

(provide 'init-python)
;;; init-python.el ends here
