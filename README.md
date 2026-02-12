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
uv tool install pyrefly
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

create the build config files in `~/.config/emacs-plus/build.yml` at first

``` yaml
patches:
  - aggressive-read-buffering
  - mac-font-use-typo-metrics
icon: dragon-plus
inject_path: true
```

then install with the following instructions:

``` bash
brew tap d12frosted/emacs-plus
brew install emacs-plus@30
cp -r /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications/
```

Then don't forget to choose your favorite font in `Options > Set Default Font` and then `Options > Save Options`
