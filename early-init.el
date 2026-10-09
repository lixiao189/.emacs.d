;;; early-init.el --- Runs before package init and the first frame -*- lexical-binding: t; -*-

;; No GC or file-name handlers during startup; restored afterwards.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar my/default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/default-file-name-handler-alist
                  gc-cons-threshold (* 64 1024 1024)   ; 64MB
                  gc-cons-percentage 0.2)))

;; Load one pre-generated autoloads file instead of scanning elpa/.
;; No `package-native-compile': it compiles every .el in a package (tests,
;; .dir-locals.el, -pkg.el) at install time; JIT compiles what actually loads.
(setq package-quickstart t)

;; Quiet async native compilation, cache kept under var/.
(setq native-comp-async-report-warnings-errors nil
      native-comp-jit-compilation t)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; Skip frame resizing and other redraw work during startup.
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
      ;; Third-party noise: elpa files without a lexical-binding cookie, and
      ;; `package-quickstart-refresh' calling `package-initialize' after each
      ;; install during init.
      warning-suppress-log-types '((comp) (bytecomp)
                                   (files missing-lexbind-cookie)
                                   (package reinitialization)))

;; Hide UI chrome before the first frame is drawn.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
;; Match Ghostty: internal padding, no native title-bar proxy icon,
;; appearance follows the system (Latte / Mocha is applied in init-ui).
(push '(internal-border-width . 8) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(setq tool-bar-mode nil
      ns-use-proxy-icon nil)

;;; early-init.el ends here
