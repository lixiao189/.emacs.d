;;; lisp-apheleia.el --- Apheleia formatting -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package apheleia
  :straight t)

(defun +format-buffer ()
  "Format current buffer with Apheleia, or fall back to Eglot."
  (interactive)
  (require 'apheleia)
  (let ((formatters (apheleia--get-formatters)))
    (cond
     (formatters
      (apheleia-format-buffer formatters))
     ((progn
        (require 'eglot nil t)
        (fboundp 'eglot-format-buffer))
      (eglot-format-buffer))
     (t
      (user-error "No formatter configured (Apheleia or Eglot)")))))

(provide 'lisp-apheleia)
;;; lisp-apheleia.el ends here
