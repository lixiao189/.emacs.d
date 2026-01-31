;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package python
  :straight nil
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-shell-dedicated 'project)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (with-eval-after-load 'apheleia
    (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff))
  (with-eval-after-load 'eglot
    ;; Use ty as the Python language server.
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode) . ("ty" "server")))))

(provide 'init-python)
;;; init-python.el ends here
