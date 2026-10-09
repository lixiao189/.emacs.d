# Emacs config (macOS, Emacs 31)

Fast-starting Emacs with Vim emulation, LSP (eglot) for C++/Go/Python/Rust, and LazyVim's keymaps.

## First run
Packages install automatically. Then:
1. Font: `brew install --cask font-maple-mono-nf-cn` (Maple Mono NF CN, 14pt; covers CJK).
2. Install servers you lack: `brew install llvm` / `go install golang.org/x/tools/gopls@latest` /
   `uv tool install ty ruff` for Python;
   `rustup component add rust-analyzer rustfmt clippy` for Rust.
3. Copilot: `M-x copilot-install-server`, then `M-x copilot-login` (node from fnm's default alias).
4. Optional speed-up: `cargo install emacs-lsp-booster` (auto-detected).

C++ needs `compile_commands.json` (`cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON`).

## Keys
Leader is `SPC` (`M-SPC` in insert mode). which-key shows the menus. Same groups as LazyVim:
`<leader>f` file, `s` search, `c` code, `g` git, `b` buffer, `w` window, `x` diagnostics,
`u` toggles, `q` quit, `<tab>` tabs. Non-leader: `gd gr gI gy K`, `]d [d ]e [e ]w [w`, `H/L`
buffers, `C-h/j/k/l` windows, `SPC e`/`SPC E` toggle the file tree (project root / cwd), `M-j/M-k` move lines, `C-s` save, `s` jump.
