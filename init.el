;;; init.el --- Entry point -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))

(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

(require 'init-core)
(require 'init-ui)
(require 'init-evil)
(require 'init-completion)
(require 'init-git)
(require 'init-lsp)
(require 'init-cpp)
(require 'init-go)
(require 'init-python)
(require 'init-keys)

(when (file-exists-p custom-file) (load custom-file nil 'nomessage))

;;; init.el ends here
