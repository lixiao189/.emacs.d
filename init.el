;;; init.el --- Entry point -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))

(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

;; Read when evil loads. On first run, installing treemacs-evil (init-ui)
;; loads evil before init-evil, so set these before any module.
(setq evil-want-integration t
      evil-want-keybinding nil)           ; evil-collection handles it

(require 'init-core)
(require 'init-ui)
(require 'init-evil)
(require 'init-completion)
(require 'init-copilot)
(require 'init-git)
(require 'init-term)
(require 'init-lsp)
(require 'init-cpp)
(require 'init-go)
(require 'init-python)
(require 'init-rust)
(require 'init-markdown)
(require 'init-keys)

(when (file-exists-p custom-file) (load custom-file nil 'nomessage))

;;; init.el ends here
