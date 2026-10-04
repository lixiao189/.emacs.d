;;; init-cpp.el --- C / C++ -*- lexical-binding: t; -*-

(setq c-basic-offset 2)

(my/lang-setup '(c-mode c++-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((c-mode c++-mode)
                 . ,(eglot-alternatives
                     '(("clangd" "--background-index" "--clang-tidy"
                        "--header-insertion=iwyu" "--completion-style=detailed"
                        "--function-arg-placeholders=false" "-j=4"
                        "--fallback-style=llvm")
                       "ccls")))))

(provide 'init-cpp)
;;; init-cpp.el ends here
