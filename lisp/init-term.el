;;; init-term.el --- Terminal emulator -*- lexical-binding: t; -*-

;; vterm: libvterm-based, a real terminal (TUIs, colors, fast output).
;; The native module is built with cmake on first use, without asking.
(use-package vterm
  :commands vterm
  :init
  (setq vterm-always-compile-module t
        vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t)
  ;; Show terminals in a split at the bottom of the frame.  Match the
  ;; name: vterm displays its buffer before enabling `vterm-mode'.
  (add-to-list 'display-buffer-alist
               '("\\`\\*vterm"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.3)))
  :config
  ;; C-d exits the shell; `vterm-kill-buffer-on-exit' kills the buffer but
  ;; leaves its bottom split showing another buffer.  Delete that window too.
  (defun my/vterm-delete-window-on-exit (buffer _event)
    (when-let* ((win (get-buffer-window buffer)))
      (unless (one-window-p 'nomini (window-frame win))
        (delete-window win))))
  (add-hook 'vterm-exit-functions #'my/vterm-delete-window-on-exit))

(provide 'init-term)
;;; init-term.el ends here
