;;; init-lang.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

;; Apheleia formatting
(use-package apheleia
  :ensure t
  :custom
  (apheleia-mode-alist nil)
  (apheleia-remote-algorithm 'remote))

(defun +format-buffer ()
  "Format current buffer with Apheleia, or fall back to lsp-mode."
  (interactive)
  (require 'apheleia)
  (let ((formatters (apheleia--get-formatters)))
    (cond
     (formatters
      (apheleia-format-buffer formatters))
     ((progn
        (require 'lsp-mode nil t)
        (fboundp 'lsp-format-buffer))
      (lsp-format-buffer))
     (t
      (user-error "No formatter configured (Apheleia or lsp-mode)")))))

;; The completion engine
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  (company-tooltip-maximum-width 80)
  (company-backends '((company-capf company-yasnippet)
                      company-files))
  :hook ((prog-mode . company-mode)
         (shell-mode . company-mode)
         (eshell-mode . company-mode)))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-keep-workspace-alive nil)
  (lsp-warn-no-matched-clients nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-enable-hover nil)

  ;; Performance
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-enable-file-watchers nil))

;; The unified debugger
(use-package gud
  :ensure nil
  :hook (gud-mode . gud-tooltip-mode)
  :custom
  (gud-highlight-current-line t))

;; GDB specific config
(use-package gdb-mi
  :ensure nil
  :commands gdb
  :custom
  (gdb-show-main t)
  (gdb-display-io-nopopup t)
  (gdb-show-changed-values t)
  (gdb-delete-out-of-scope t)
  (gdb-use-colon-colon-notation t)
  (gdb-debuginfod-enable-setting nil)
  (gdb-restore-window-configuration-after-quit t))

;; #number can be clickable.
(use-package bug-reference
  :ensure nil
  :bind (:map bug-reference-map
         ("C-c C-o" . bug-reference-push-button)))

;; Highlight TODO
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t i" . hl-todo-insert)
         ("C-c t o" . hl-todo-occur)
         ("C-c t s" . hl-todo-rgrep)))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

;; xref
(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (xref-search-program (cond ((executable-find "rg") 'ripgrep)
                             ((executable-find "ugrep") 'ugrep)
                             (t 'grep)))
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-show-indicators t)
  (hs-display-lines-hidden t)
  (hs-indicator-type (if (display-graphic-p) 'fringe 'margin))
  (hs-hide-comments-when-hiding-all nil))

;; Tree-sitter auto mode with automatic grammar installation
(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  ;; Automatically install grammars when missing
  (treesit-auto-install 't)
  :config
  (global-treesit-auto-mode))

(require 'init-cpp)
(require 'init-rust)
(require 'init-python)
(require 'init-text)
(require 'init-elisp)

(provide 'init-lang)

;;; init-lang.el ends here
