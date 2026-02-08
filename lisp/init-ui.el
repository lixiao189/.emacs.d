;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; relative number
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'visual)

;; Use Iosvkem in terminals
(use-package doom-themes
  :straight t
  :config
  (doom-themes-org-config)

  (defun auto-change-theme (&optional frame)
    "Load different theme based on FRAME background mode."
    (when frame (select-frame frame))
    (let ((theme (if (eq (frame-parameter nil 'background-mode) 'light)
                   'doom-oksolar-light
                   'doom-oksolar-dark)))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)))

  (add-hook 'after-init-hook #'auto-change-theme)
  (add-hook 'after-make-frame-functions #'auto-change-theme))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-github nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

;; Customize popwin behavior
(use-package shackle
  :straight t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((vc-annotate-mode         :select t :inhibit-window-quit t :same t)
                   ("*quickrun*"             :select t :inhibit-window-quit t :same t)
                   (profiler-report-mode     :select t)
                   (xwidget-webkit-mode      :select t :same t)
                   (comint-mode              :select t :align t :size 0.4)
                   (grep-mode                :select t :align t)
                   (rg-mode                  :select t :align t)
                   ;; See also `help-window-select'
                   (apropos-mode             :select nil :align t :size 0.4)
                   (help-mode                :select nil :align t :size 0.4)
                   ("*Backtrace*"               :select t   :align t :size 15)
                   ("*Shell Command Output*"    :select nil :align t :size 0.4)
                   ("*Async Shell Command*"     :select nil :align t :size 0.4)
                   ("*Org-Babel Error Output*"  :select nil :align t :size 0.3)
                   ("*Process List*"            :select t   :align t :size 0.3)
                   ("*Occur*"                   :select t   :align t)
                   ("\\*eldoc\\( for \\)?.*\\*" :select t   :align t :size 15 :regexp t))))

(use-package help
  :straight nil
  :custom
  (help-window-select t)
  (help-window-keep-selected t)
  (help-enable-variable-value-editing t))

;; Windows layout recorder
;;
;; You can still use `winner-mode' on Emacs 26 or early. On Emacs 27, it's
;; preferred over `winner-mode' for better compatibility with `tab-bar-mode'.
(use-package tab-bar
  :straight nil
  :hook (after-init . tab-bar-history-mode)
  :custom
  (tab-bar-history-buttons-show nil))

(use-package nerd-icons
  :straight t
  :when (display-graphic-p)
  :demand t)

(provide 'init-ui)

;;; init-ui.el ends here
