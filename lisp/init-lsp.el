;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package eglot-booster
	:straight ( eglot-booster :type git :host nil :repo "https://github.com/jdtsmith/eglot-booster")
	:after eglot
	:config (eglot-booster-mode))
(setq eglot-booster-io-only t)

;; The completion engine
(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode))

(use-package corfu
  :straight t
  :hook ((prog-mode . corfu-mode)
         (ielm-mode . corfu-mode))
  :bind (:map corfu-mode-map
         ([remap completion-at-point] . corfu-complete)
         :map corfu-map
         ([tab] . corfu-next)
         ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-quit-no-match t))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c f" . eglot-format)
         ("C-c d" . eldoc-doc-buffer)
         ("C-c a" . eglot-code-actions)
         ("C-c r" . eglot-rename)
         ("C-c l" . eglot-command-map))
  :config
  (defvar-keymap eglot-command-map
    :prefix 'eglot-command-map
    ;; workspaces
    "w q" #'eglot-shutdown
    "w r" #'eglot-reconnect
    "w s" #'eglot
    "w d" #'eglot-show-workspace-configuration

    ;; formatting
    "= =" #'eglot-format-buffer
    "= r" #'eglot-format

    ;; goto
    "g a" #'xref-find-apropos
    "g d" #'eglot-find-declaration
    "g g" #'xref-find-definitions
    "g i" #'eglot-find-implementation
    "g r" #'xref-find-references
    "g t" #'eglot-find-typeDefinition

    ;; actions
    "a q" #'eglot-code-action-quickfix
    "a r" #'eglot-code-action-rewrite
    "a i" #'eglot-code-action-inline
    "a e" #'eglot-code-action-extract
    "a o" #'eglot-code-action-organize-imports)
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-ignored-server-capabilities '(:documentLinkProvider
                                       :documentOnTypeFormattingProvider
                                       :foldingRangeProvider
                                       :colorProvider
                                       :inlayHintProvider)))

(provide 'init-lsp)
;;; init-lsp.el ends here
