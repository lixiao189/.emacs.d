;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Defer garbage collection further back in the startup process
(if noninteractive  ; in CLI sessions
    (setq gc-cons-threshold #x8000000   ; 128MB
          ;; Backport from 29 (see emacs-mirror/emacs@73a384a98698)
          gc-cons-percentage 1.0)
  (setq gc-cons-threshold most-positive-fixnum))

;; Faster to disable these here (before they've been initialized)
(push '(width . 160) default-frame-alist)
(push '(height . 60) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here
