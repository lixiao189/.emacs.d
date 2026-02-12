;;; init-lang.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(use-package eglot-booster
  :after eglot
  :vc (:url "https://github.com/jdtsmith/eglot-booster"
       :rev :newest)
  :config
  (eglot-booster-mode)
  :custom
  (eglot-booster-io-only t))

;; Apheleia formatting
(use-package apheleia
  :ensure t)

(defun +format-buffer ()
  "Format current buffer with Apheleia, or fall back to Eglot."
  (interactive)
  (require 'apheleia)
  (let ((formatters (apheleia--get-formatters)))
    (cond
     (formatters
      (apheleia-format-buffer formatters))
     ((progn
        (require 'eglot nil t)
        (fboundp 'eglot-format-buffer))
      (eglot-format-buffer))
     (t
      (user-error "No formatter configured (Apheleia or Eglot)")))))

;; The completion engine
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-auto-trigger ".")
  (corfu-quit-no-match 'separator)

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (corfu-mode . (lambda ()
                       (setq-local completion-styles '(basic))
                       (setq-local completion-category-defaults nil)
                       (setq-local completion-category-overrides nil))))

  :init
  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package kind-icon
  :ensure t
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :config
  (defvar-keymap eglot-command-map
    :prefix 'eglot-command-map
    ;; goto
    "g a" #'xref-find-apropos
    "g D" #'eglot-find-declaration
    "g d" #'xref-find-definitions
    "g i" #'eglot-find-implementation
    "g r" #'xref-find-references
    "g y" #'eglot-find-typeDefinition)
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-report-progress 'messages)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-ignored-server-capabilities '(:documentLinkProvider
                                       :documentOnTypeFormattingProvider
                                       :colorProvider
                                       :inlayHintProvider
                                       :semanticTokensProvider)))

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

(require 'init-cpp)
(require 'init-rust)
(require 'init-python)
(require 'init-text)
(require 'init-elisp)

(provide 'init-lang)

;;; init-lang.el ends here
