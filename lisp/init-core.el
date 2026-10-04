;;; init-core.el --- Packages, macOS, sane defaults, performance -*- lexical-binding: t; -*-

;;;; Packages
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

;;;; macOS

;; Left Option = Meta, right Option types symbols, Cmd = Super.
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

;; Set PATH by hand; exec-path-from-shell is too slow at startup.
(dolist (dir (list "/opt/homebrew/bin" "/opt/homebrew/sbin" "/usr/local/bin"
                   "/opt/homebrew/opt/rustup/bin" ; rust-analyzer, rustfmt
                   (expand-file-name "~/.local/bin")
                   (expand-file-name "~/go/bin")
                   (expand-file-name "~/.cargo/bin")
                   (expand-file-name "~/.local/share/fnm/aliases/default/bin") ; node (copilot, npm servers)
                   (expand-file-name "~/.emacs.d/bin")))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (unless (string-match-p (regexp-quote dir) (getenv "PATH"))
      (setenv "PATH" (concat dir ":" (getenv "PATH"))))))

;; Prefer GNU ls (gls) for --group-directories-first.
(if-let* ((gls (executable-find "gls")))
    (setq insert-directory-program gls
          dired-listing-switches "-alh --group-directories-first")
  (setq dired-use-ls-dired nil
        dired-listing-switches "-alh"))

;;;; Performance
(setq read-process-output-max (* 1024 1024)   ; LSP sends large JSON
      process-adaptive-read-buffering t
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      bidi-inhibit-bpa t
      auto-mode-case-fold nil
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      idle-update-delay 1.0
      jit-lock-stealth-time nil
      vc-handled-backends '(Git)             ; don't probe other VCSs
      ffap-machine-p-known 'reject
      inhibit-x-resources t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;;;; Files and state
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
      tramp-persistency-file-name (concat my/var-dir "tramp")
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

;; Enable these after startup to keep load time down.
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
