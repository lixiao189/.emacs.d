;;; init-web.el --- Web Development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; JavaScript
(use-package js
  :ensure nil
  :mode ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2)
  :config
  ;; Ensure buffers inherit the intended default.
  (setq-default js-indent-level 2)
  (with-eval-after-load 'apheleia
    (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier-javascript)
    (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier-javascript)))

;; JSON
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (with-eval-after-load 'apheleia
    (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier-json)
    (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier-json)))

;; Configure Prettier formatters
(with-eval-after-load 'apheleia
  ;; Add Prettier formatters
  (setf (alist-get 'prettier-javascript apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'prettier-typescript apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'prettier-html apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'prettier-css apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath)))

(provide 'init-web)
;;; init-web.el ends here
