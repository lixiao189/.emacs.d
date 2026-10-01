;;; init-go.el --- Go -*- lexical-binding: t; -*-

(my/add-grammars
 '(go    "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
 '(gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0"))

(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.work\\'" . go-work-ts-mode))

(setq go-ts-mode-indent-offset 4)

(defun my/go-use-tabs-h ()
  (setq tab-width 4
        indent-tabs-mode t))
(add-hook 'go-mode-hook #'my/go-use-tabs-h)
(add-hook 'go-ts-mode-hook #'my/go-use-tabs-h)

(my/lang-setup '(go-mode go-ts-mode go-mod-ts-mode))

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
    (dolist (mode '(go-mode go-ts-mode))
      (setf (alist-get mode apheleia-mode-alist) formatter))))

(provide 'init-go)
;;; init-go.el ends here
