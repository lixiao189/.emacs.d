# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal Emacs 31 config for macOS: Vim emulation (evil), LSP via eglot (C++/Go/Python/Rust), LazyVim-style `SPC` leader keymaps. See `README.md` for the per-file table and first-run setup (grammar install via `M-x my/treesit-install-grammars`, LSP servers via brew/go/npm).

## Load order and architecture

- `early-init.el` → `init.el` → `lisp/init-*.el` (language modules live in `lisp/lang/`), required in this order: core, ui, evil, completion, git, lsp, cpp, go, python, rust, keys. `init-keys.el` must stay last: it uses `general` `:after evil` and binds commands from consult/eglot/etc. defined by earlier modules.
- Every package uses `use-package` with `use-package-always-ensure t` and `use-package-always-defer t`, so packages are lazy by default. Add `:demand t` (or a hook/command/keymap trigger) if something must load at startup.
- Startup speed is a design goal. Do not add `exec-path-from-shell`. PATH is set by hand in `init-core.el`. GC and file-handler tuning lives in `early-init.el` and is restored on `emacs-startup-hook`.
- Custom helper functions and leader definitions use the `my/` prefix (`my/leader`, `my/root`, ...).
- Runtime state is kept out of the config tree under `var/` (eln-cache, recentf, savehist, projects, `custom.el`). Customize output goes to `var/custom.el`, loaded at the end of `init.el`.
- `package-quickstart.el` and `.elc` are generated files (`package-quickstart t`). Do not hand-edit them. Refresh with `M-x package-quickstart-refresh` after package changes. `elpa/`, `tree-sitter/` and `var/` are installed or generated artifacts.
- Tree-sitter: grammars are listed in `treesit-language-source-alist` in `init-lsp.el` (shared) and each `lisp/lang/init-<lang>.el`. `treesit-enabled-modes t` remaps `*-mode` to `*-ts-mode` when a grammar is available (Emacs 31 feature). Add a new language as its own `lisp/lang/init-<lang>.el` (grammar, indent, eglot server, formatter, `my/lang-setup`).

## Commands

There is no build or test suite. Validate changes with:

```bash
emacs --batch -l ~/.emacs.d/early-init.el --eval "(package-activate-all)" -l ~/.emacs.d/init.el   # load errors show up on stderr
emacs --batch -f batch-byte-compile lisp/init-keys.el              # check one file for warnings (delete the .elc after)
```

Reload a module in a running Emacs with `M-x load-file`.
