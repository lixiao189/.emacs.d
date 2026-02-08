;;; init-mini.el --- The minimal configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs minimal configuration for debugging.
;;

;;; Code:

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bootstrap `use-package'
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(eval-and-compile
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

(setq debug-on-error t)
(setq-default lexical-binding t)

(add-to-list 'load-path (file-name-as-directory (locate-user-emacs-file "lisp")))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-base)

;; Incremental complete in minibuffer
(use-package icomplete
  :straight nil
  :hook (emacs-startup . icomplete-mode)
  :custom
  (icomplete-vertical-mode t)
  (icomplete-prospects-height 10)
  (icomplete-hide-common-prefix nil)
  (icomplete-show-matches-on-no-input nil))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-mini)
;;; init-mini.el ends here
