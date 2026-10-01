;;; init-python.el --- Python -*- lexical-binding: t; -*-

(my/add-grammars
 '(python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))

(setq python-indent-offset 4
      python-indent-guess-indent-offset nil)

(my/lang-setup '(python-mode python-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 . ,(eglot-alternatives
                     '(("basedpyright-langserver" "--stdio")
                       ("pyright-langserver" "--stdio")
                       "pylsp"
                       ("ruff" "server")))))
  (my/eglot-workspace-config
   :basedpyright.analysis '(:typeCheckingMode "standard")))

(with-eval-after-load 'apheleia
  (dolist (mode '(python-mode python-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) '(ruff-isort ruff))))

(provide 'init-python)
;;; init-python.el ends here
