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

(with-eval-after-load 'lsp-mode
  ;; Use ty as the Python language server.
  (require 'lsp-protocol)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ty" "server"))
    :activation-fn (lsp-activate-on "python")
    :priority 1
    :server-id 'ty-lsp)))

(provide 'init-python)
;;; init-python.el ends here
