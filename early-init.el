;;; early-init.el --- Runs before package init and the first frame -*- lexical-binding: t; -*-

;; Startup: no GC during init, no file-handler lookups, restored afterwards.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar my/default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/default-file-name-handler-alist
                  gc-cons-threshold (* 64 1024 1024)   ; 64MB: fewer GC pauses while typing
                  gc-cons-percentage 0.2)))

;; Packages: quickstart = one pre-generated autoload file instead of scanning elpa/.
(setq package-quickstart t
      package-native-compile t)

;; Native compilation: quiet, async, keep the eln cache out of the config dir.
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-jit-compilation t)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; Avoid frame resizes / redraws / fonts work while starting up.
(setq frame-inhibit-implied-resize t
      inhibit-compacting-font-caches t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      use-file-dialog nil
      use-dialog-box nil
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp)))

;; UI chrome off before the first frame is drawn (no flash, no relayout).
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(setq tool-bar-mode nil)

;;; early-init.el ends here
