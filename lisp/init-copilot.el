;;; init-copilot.el --- GitHub Copilot inline suggestions -*- lexical-binding: t; -*-

;; VS Code style ghost text: Tab accepts, C-<right> accepts the next word,
;; M-] / M-[ cycle suggestions, C-g dismisses. The completion popup keeps
;; Tab while it is open, like VS Code's suggest widget.
;; First run: M-x copilot-install-server, then M-x copilot-login.
(use-package copilot
  :hook ((prog-mode text-mode conf-mode) . copilot-mode)
  :init
  (setq copilot-install-dir (expand-file-name "var/copilot" user-emacs-directory)
        copilot-indent-offset-warning-disable t
        ;; Big files are sent as a `copilot-max-char' window around point,
        ;; which is plenty of context; don't warn about it.
        copilot-max-char-warning-disable t)
  :config
  (defun my/copilot-tab ()
    "Accept the popup candidate if the popup is open, else the Copilot suggestion."
    (interactive)
    (if (bound-and-true-p completion-in-region-mode)
        (corfu-insert)
      (copilot-accept-completion)))
  ;; The overlay keymap is only active while ghost text is shown and takes
  ;; precedence over evil's insert-state map.
  (keymap-set copilot-completion-map "TAB" #'my/copilot-tab)
  (keymap-set copilot-completion-map "<tab>" #'my/copilot-tab)
  (keymap-set copilot-completion-map "C-<right>" #'copilot-accept-completion-by-word)
  (keymap-set copilot-completion-map "C-e" #'copilot-accept-completion-by-line)
  (keymap-set copilot-completion-map "M-]" #'copilot-next-completion)
  (keymap-set copilot-completion-map "M-[" #'copilot-previous-completion)
  (keymap-set copilot-completion-map "C-g" #'copilot-clear-overlay))

(provide 'init-copilot)
;;; init-copilot.el ends here
