# File structure

Personal Emacs 31 config for macOS. Load order is fixed by `init.el`:

`early-init.el` → `init.el` → `lisp/init-*.el` (language modules in `lisp/lang/`).

`init-keys.el` must stay last: it uses `general` `:after evil` and binds commands from consult/eglot/etc. defined by earlier modules.

| File | Purpose |
|---|---|
| `early-init.el` | GC/file-handler tricks, frame chrome off, native-comp, package quickstart |
| `init.el` | `load-path`, `custom-file`, evil integration flags, then the `require`s below |
| `lisp/init-core.el` | package setup, macOS modifiers/PATH, perf variables, state files under `var/` |
| `lisp/init-ui.el` | Catppuccin (Latte/Mocha), font, relative line numbers, which-key, dashboard (`c` opens the config folder), treemacs file tree (`SPC e`) |
| `lisp/init-evil.el` | evil, evil-collection, surround (`gsa/gsd/gsr`), comment (`gc`), avy (`s`) |
| `lisp/init-completion.el` | vertico, orderless, marginalia, consult, embark, corfu, cape |
| `lisp/init-copilot.el` | GitHub Copilot ghost text: `TAB` accepts (popup first if open), `C-<right>` word, `C-e` line, `M-]`/`M-[` cycle, `C-g` dismiss |
| `lisp/init-git.el` | magit (`SPC g g`), diff-hl gutter |
| `lisp/init-term.el` | vterm terminal (`SPC f t` at the project root; module builds with cmake on first use) |
| `lisp/init-lsp.el` | shared eglot and apheleia setup |
| `lisp/lang/init-cpp.el` | C/C++: 2-space indent, clangd |
| `lisp/lang/init-go.el` | Go: go-mode, tabs, gopls settings, goimports |
| `lisp/lang/init-python.el` | Python: ty (LSP), ruff linting (flymake-ruff) and formatting |
| `lisp/lang/init-rust.el` | Rust: rust-mode, rust-analyzer (clippy checks), rustfmt |
| `lisp/lang/init-markdown.el` | Markdown: markdown-mode, live preview with KaTeX math in an xwidget side window that scrolls with the editor (`SPC c p`) |
| `lisp/init-keys.el` | LazyVim keymaps (`SPC` leader) |

## Conventions

- Every package uses `use-package` with `use-package-always-ensure t` and `use-package-always-defer t`, so packages are lazy by default. Add `:demand t` (or a hook/command/keymap trigger) if something must load at startup.
- Custom helper functions and leader definitions use the `my/` prefix (`my/leader`, `my/root`, ...).
- Tree-sitter is not used; languages use classic major modes. Add a new language as its own `lisp/lang/init-<lang>.el` (major-mode package if needed, indent, eglot server, `my/lang-setup`, `my/eglot-workspace-config`, formatter) and `require` it from `init.el` before `init-keys`.

## Generated and runtime state

Not part of the config. Do not hand-edit.

| Path | What it is |
|---|---|
| `var/` | runtime state: eln-cache, recentf, savehist, projects, `custom.el` (loaded at the end of `init.el`) |
| `elpa/` | installed packages |
| `eln-cache/` | leftover native-comp cache; the live cache is redirected to `var/eln-cache/` |
| `package-quickstart.el` / `.elc` | generated autoloads (`package-quickstart t`); refresh with `M-x package-quickstart-refresh` after package changes |
| `*.elc` | byte-compiled output |

Startup speed is a design goal. Do not add `exec-path-from-shell`. PATH is set by hand in `init-core.el`. GC and file-handler tuning lives in `early-init.el` and is restored on `emacs-startup-hook`.
