;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

;; C/C++ Mode
(use-package cc-mode
  :init (setq-default c-basic-offset 2)
  :custom
  (lsp-clients-clangd-args '("--log=error")))

;; cmake, the de factor build system for C++
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

;; Extra font locks for cmake
(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

(provide 'init-cpp)

;;; init-cpp.el ends here
