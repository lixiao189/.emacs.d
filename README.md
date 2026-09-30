# Emacs config (macOS, Emacs 31)

Fast-starting Emacs with Vim emulation, LSP (eglot) for C++/Go/Python, and LazyVim's keymaps.

| File | Purpose |
|---|---|
| `early-init.el` | GC/file-handler tricks, frame chrome off, native-comp, package quickstart |
| `lisp/init-core.el` | package setup, macOS modifiers/PATH, perf variables, state files under `var/` |
| `lisp/init-ui.el` | doom-tokyo-night theme, font, relative line numbers, which-key, dashboard (`c` opens the config folder) |
| `lisp/init-evil.el` | evil, evil-collection, surround (`gsa/gsd/gsr`), comment (`gc`), avy (`s`) |
| `lisp/init-completion.el` | vertico, orderless, marginalia, consult, embark, corfu, cape |
| `lisp/init-git.el` | magit (`SPC g g`), diff-hl gutter |
| `lisp/init-lsp.el` | shared tree-sitter, eglot and apheleia setup |
| `lisp/lang/init-cpp.el` | C/C++: grammar, 2-space indent, clangd |
| `lisp/lang/init-go.el` | Go: grammar, tabs, gopls settings, goimports |
| `lisp/lang/init-python.el` | Python: grammar, basedpyright, ruff formatting |
| `lisp/init-keys.el` | LazyVim keymaps (`SPC` leader) |

## First run
Packages install automatically. Then:
1. `M-x my/treesit-install-grammars` (C, C++, Go, gomod, Python, ...).
2. Install servers you lack: `brew install llvm` / `go install golang.org/x/tools/gopls@latest` /
   `npm i -g basedpyright` (or `pipx install basedpyright`); `ruff` is used for Python formatting.
3. Optional speed-up: `cargo install emacs-lsp-booster` (auto-detected).

C++ needs `compile_commands.json` (`cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON`).

## Keys
Leader is `SPC` (`M-SPC` in insert mode). which-key shows the menus. Same groups as LazyVim:
`<leader>f` file, `s` search, `c` code, `g` git, `b` buffer, `w` window, `x` diagnostics,
`u` toggles, `q` quit, `<tab>` tabs. Non-leader: `gd gr gI gy K`, `]d [d ]e [e ]w [w`, `H/L`
buffers, `C-h/j/k/l` windows, `M-j/M-k` move lines, `C-s` save, `s` jump.
