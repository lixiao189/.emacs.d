;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package python
  :ensure nil
  :custom
  (python-shell-dedicated 'project)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (with-eval-after-load 'apheleia
    (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)))

(provide 'init-python)
;;; init-python.el ends here
