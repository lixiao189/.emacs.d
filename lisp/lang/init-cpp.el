;;; init-cpp.el --- C / C++ -*- lexical-binding: t; -*-

(my/add-grammars
 '(c   "https://github.com/tree-sitter/tree-sitter-c" "v0.23.4")
 '(cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))

(setq c-ts-mode-indent-offset 2
      c-ts-mode-indent-style 'k&r
      c-basic-offset 2)                 ; for non-tree-sitter c-mode/c++-mode

(my/lang-setup '(c-mode c-ts-mode c++-mode c++-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ,(eglot-alternatives
                     '(("clangd" "--background-index" "--clang-tidy"
                        "--header-insertion=iwyu" "--completion-style=detailed"
                        "--function-arg-placeholders=false" "-j=4"
                        "--fallback-style=llvm")
                       "ccls")))))

(provide 'init-cpp)
;;; init-cpp.el ends here
