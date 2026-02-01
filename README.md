# Emacs config backup

## Preinstall emacs

``` bash
cargo install emacs-lsp-booster
brew install gnu-tar # needed on mac
brew install rg
brew install fd

# tools to install lsp
brew install uv
brew install bun
```

## Install lsp

``` bash
# python
uv tool install ty
uv tool install ruff

# c++
brew install clangd

# rust
rustup component add rust-analyzer

# javascript
bun add -g typescript-language-server
bun add -g prettier
```

## Install emacs on mac os

### emacs-mac

``` bash
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-natural-title-bar --with-native-compilation --with-xwidgets --with-mac-metal # metal is an experimental feature
brew pin emacs-mac # Freeze the version
cp -a $(brew --prefix)/opt/emacs-mac/Emacs.app /Applications
```

### emacs-plus (recommand)

``` bash
brew tap d12frosted/emacs-plus
brew install --cask d12frosted/emacs-plus/emacs-plus-app
```
