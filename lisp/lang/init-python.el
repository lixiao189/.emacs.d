;;; init-python.el --- Python -*- lexical-binding: t; -*-

(setq python-indent-offset 4
      python-indent-guess-indent-offset nil)

(my/lang-setup '(python-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives
                     '(("basedpyright-langserver" "--stdio")
                       ("pyright-langserver" "--stdio")
                       "pylsp"
                       ("ruff" "server")))))
  (my/eglot-workspace-config
   :basedpyright.analysis '(:typeCheckingMode "standard")))

(with-eval-after-load 'apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff)))

(provide 'init-python)
;;; init-python.el ends here
