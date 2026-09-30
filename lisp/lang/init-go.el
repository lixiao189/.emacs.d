;;; init-go.el --- Go -*- lexical-binding: t; -*-

(add-to-list 'treesit-language-source-alist
             '(go    "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4"))
(add-to-list 'treesit-language-source-alist
             '(gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0"))

(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.work\\'" . go-work-ts-mode))

(setq go-ts-mode-indent-offset 4)
(add-hook 'go-ts-mode-hook (lambda () (setq tab-width 4 indent-tabs-mode t)))
(add-hook 'go-mode-hook    (lambda () (setq tab-width 4 indent-tabs-mode t)))

(my/lang-setup '(go-mode go-ts-mode) t)
(add-hook 'go-mod-ts-mode-hook #'eglot-ensure)

;; gopls is eglot's built-in default server.
(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                (plist-put
                 (default-value 'eglot-workspace-configuration)
                 :gopls '(:staticcheck t
                          :usePlaceholders :json-false
                          :completeUnimported t
                          :analyses (:unusedparams t :shadow t)
                          :hints (:assignVariableTypes t :compositeLiteralFields t
                                  :compositeLiteralTypes t :constantValues t
                                  :functionTypeParameters t :parameterNames t
                                  :rangeVariableTypes t)))))

(with-eval-after-load 'apheleia
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  (dolist (m '(go-mode go-ts-mode))
    (setf (alist-get m apheleia-mode-alist)
          (if (executable-find "goimports") 'goimports 'gofmt))))

(provide 'init-go)
;;; init-go.el ends here
