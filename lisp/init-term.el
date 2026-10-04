;;; init-term.el --- Terminal emulator -*- lexical-binding: t; -*-

;; vterm: libvterm-based, a real terminal (TUIs, colors, fast output).
;; The native module is built with cmake on first use, without asking.
(use-package vterm
  :commands vterm
  :init
  (setq vterm-always-compile-module t
        vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t))

(provide 'init-term)
;;; init-term.el ends here
