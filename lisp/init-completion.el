;;; init-completion.el --- Minibuffer & in-buffer completion, search -*- lexical-binding: t; -*-

(use-package vertico
  :hook (after-init . vertico-mode)
  :init
  (setq vertico-cycle t vertico-count 12)
  :config
  (require 'vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  ;; Telescope-style movement.
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous)
  (keymap-set vertico-map "C-d" #'vertico-scroll-up)
  (keymap-set vertico-map "C-u" #'vertico-scroll-down))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :commands (consult-buffer consult-ripgrep consult-fd consult-line consult-imenu
             consult-flymake consult-recent-file consult-theme consult-mark
             consult-register consult-yank-pop consult-complex-command
             consult-info consult-man consult-xref)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  :config
  (setq consult-preview-key "M-."           ; no live preview = snappier
        consult-narrow-key "<"
        consult-async-min-input 2
        consult-async-refresh-delay 0.1
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1))

(use-package embark
  :commands (embark-act embark-dwim embark-export)
  :init (setq prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult :after (embark consult))

;; In-buffer completion popup (LazyVim/blink.cmp style keys).
(use-package corfu
  :hook (after-init . global-corfu-mode)
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-quit-no-match 'separator
        corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (keymap-set corfu-map "RET" nil)            ; Enter = newline, never accept
  (keymap-set corfu-map "C-y" #'corfu-insert) ; accept
  (keymap-set corfu-map "C-e" #'corfu-quit)   ; dismiss
  (keymap-set corfu-map "C-n" #'corfu-next)
  (keymap-set corfu-map "C-p" #'corfu-previous)
  (keymap-set corfu-map "C-j" #'corfu-next)
  (keymap-set corfu-map "C-k" #'corfu-previous)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'evil
    (evil-define-key 'insert 'global (kbd "C-SPC") #'completion-at-point)))

(use-package cape
  :demand t
  :hook ((prog-mode text-mode conf-mode) . my/cape-setup)
  :init
  (defun my/cape-setup ()
    (add-hook 'completion-at-point-functions #'cape-file 90 t)
    (add-hook 'completion-at-point-functions #'cape-dabbrev 95 t)))

(provide 'init-completion)
;;; init-completion.el ends here
