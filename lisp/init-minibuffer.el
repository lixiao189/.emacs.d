;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Completion engine
(use-package minibuffer
  :straight nil
  :bind (:map minibuffer-local-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-ns-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-completion-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-must-match-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-isearch-map
         ([escape] . abort-recursive-edit))
  :custom
  ;; Default minibuffer is fine-tuned since Emacs 29
  (completion-auto-help t)
  (completion-auto-select nil)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  ;; shorten " (default %s)" => " [%s]"
  (minibuffer-default-prompt-format " [%s]")
  (minibuffer-electric-default-mode t)
  ;; Don't insert completion at point into minibuffer
  (minibuffer-completion-auto-choose nil)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

;; Persist minibuffer history (used by vertico-repeat and consult)
(use-package savehist
  :straight t
  :hook (after-init . savehist-mode))

(use-package vertico
  :straight t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-cycle t)
  (vertico-resize t))

;; Orderless: powerful completion style for flexible matching
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :straight t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-c" . embark-export)
         ("C-c C-o" . embark-collect)))

(use-package consult
  :straight t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap yank-pop]               . consult-yank-pop)
         ([remap goto-line]              . consult-goto-line)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap jump-to-register]       . consult-register-load)
         ([remap point-to-register]      . consult-register-store))
  :config
  (consult-customize consult-ripgrep consult-git-grep consult-grep
                     consult-bookmark
                     consult-recent-file
                     consult-buffer
                     :preview-key "M-.")
  (consult-customize consult-line
                     :prompt "Search: ")

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-preview-key "M-.")
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
