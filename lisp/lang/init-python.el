;;; init-python.el --- Python -*- lexical-binding: t; -*-

(setq python-indent-offset 4
      python-indent-guess-indent-offset nil)

(my/lang-setup '(python-mode))

;; ty is the LSP server (types, completion, navigation). Eglot runs one
;; server per buffer, so ruff lints through flymake and formats through apheleia.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-mode "ty" "server")))

(use-package flymake-ruff
  :if (executable-find "ruff")
  :commands flymake-ruff-load)

(defun my/python-ruff-h ()
  "Add ruff diagnostics next to eglot's in Python buffers."
  (when (derived-mode-p 'python-mode)
    (flymake-ruff-load)))

;; Eglot resets the flymake backends, so add ruff after it takes over.
(add-hook 'eglot-managed-mode-hook #'my/python-ruff-h)

(with-eval-after-load 'apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff)))

(provide 'init-python)
;;; init-python.el ends here
