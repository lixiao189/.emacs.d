;;; init-core.el --- Packages, macOS, sane defaults, performance -*- lexical-binding: t; -*-

;;;; Package management ------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu" . 10) ("nongnu" . 9) ("melpa" . 5)))

(require 'use-package)
(setq use-package-always-ensure t     ; install missing packages on first run
      use-package-always-defer t      ; everything lazy unless it says otherwise
      use-package-expand-minimally t)

;;;; macOS --------------------------------------------------------------------
;; Option = Meta (left), right Option stays free for typing symbols. Cmd = Super.
(setq ns-alternate-modifier 'meta
      ns-right-alternate-modifier 'none
      ns-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-option-modifier 'none
      mac-command-modifier 'super
      ns-use-native-fullscreen nil
      ns-pop-up-frames nil
      ns-use-proxy-icon nil
      delete-by-moving-to-trash t)

;; exec-path-from-shell spawns a login shell (~100-300ms). Set PATH by hand.
(let ((dirs (list "/opt/homebrew/bin" "/opt/homebrew/sbin" "/usr/local/bin"
                  "/opt/homebrew/opt/rustup/bin" ; rust-analyzer, rustfmt (brew rustup)
                  (expand-file-name "~/.local/bin")
                  (expand-file-name "~/go/bin")
                  (expand-file-name "~/.cargo/bin")
                  (expand-file-name "~/.emacs.d/bin"))))
  (dolist (d dirs)
    (when (file-directory-p d)
      (add-to-list 'exec-path d)
      (unless (string-match-p (regexp-quote d) (getenv "PATH"))
        (setenv "PATH" (concat d ":" (getenv "PATH")))))))

;; BSD ls has no --group-directories-first; use GNU ls when available.
(if-let* ((gls (executable-find "gls")))
    (setq insert-directory-program gls
          dired-listing-switches "-alh --group-directories-first")
  (setq dired-use-ls-dired nil
        dired-listing-switches "-alh"))

;;;; Performance ---------------------------------------------------------------
(setq read-process-output-max (* 1024 1024)   ; LSP talks big JSON
      process-adaptive-read-buffering t
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      bidi-inhibit-bpa t
      auto-mode-case-fold nil
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      idle-update-delay 1.0
      jit-lock-stealth-time nil
      vc-handled-backends '(Git)             ; don't probe SVN/Hg/... on every file
      ffap-machine-p-known 'reject
      inhibit-x-resources t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;;;; Files & state ---------------------------------------------------------------
(defconst my/var-dir (expand-file-name "var/" user-emacs-directory))

(setq make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      recentf-save-file (concat my/var-dir "recentf")
      recentf-max-saved-items 200
      savehist-file (concat my/var-dir "savehist")
      save-place-file (concat my/var-dir "saveplace")
      project-list-file (concat my/var-dir "projects")
      bookmark-default-file (concat my/var-dir "bookmarks")
      eshell-directory-name (concat my/var-dir "eshell/")
      transient-history-file (concat my/var-dir "transient-history.el")
      url-configuration-directory (concat my/var-dir "url/")
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      global-auto-revert-non-file-buffers t
      auto-revert-avoid-polling t
      require-final-newline t
      sentence-end-double-space nil
      use-short-answers t
      confirm-kill-processes nil
      ring-bell-function #'ignore
      history-length 1000
      kill-do-not-save-duplicates t
      select-enable-clipboard t
      save-interprogram-paste-before-kill t
      help-window-select t
      tab-always-indent 'complete
      text-mode-ispell-word-completion nil
      read-extended-command-predicate #'command-completion-default-include-p)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 88)

;; Turn state modes on after startup so they don't add to the load time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((inhibit-message t))
              (recentf-mode 1)
              (savehist-mode 1)
              (save-place-mode 1)
              (global-auto-revert-mode 1)
              (winner-mode 1)
              (electric-pair-mode 1)
              (repeat-mode 1))))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(provide 'init-core)
;;; init-core.el ends here
