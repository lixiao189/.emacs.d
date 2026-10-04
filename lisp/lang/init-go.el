;;; init-go.el --- Go -*- lexical-binding: t; -*-

(use-package go-mode)                   ; also provides go.mod's go-dot-mod-mode

(defun my/go-use-tabs-h ()
  (setq tab-width 4
        indent-tabs-mode t))
(add-hook 'go-mode-hook #'my/go-use-tabs-h)

(my/lang-setup '(go-mode go-dot-mod-mode))

;; gopls is eglot's default server for Go.
(with-eval-after-load 'eglot
  (my/eglot-workspace-config
   :gopls '(:staticcheck t
            :usePlaceholders :json-false
            :completeUnimported t
            :analyses (:unusedparams t :shadow t)
            :hints (:assignVariableTypes t :compositeLiteralFields t
                    :compositeLiteralTypes t :constantValues t
                    :functionTypeParameters t :parameterNames t
                    :rangeVariableTypes t))))

(with-eval-after-load 'apheleia
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  (let ((formatter (if (executable-find "goimports") 'goimports 'gofmt)))
    (setf (alist-get 'go-mode apheleia-mode-alist) formatter)))

(provide 'init-go)
;;; init-go.el ends here
