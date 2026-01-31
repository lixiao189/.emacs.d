# Emacs config backup

## Preinstall emacs

``` bash
cargo install emacs-lsp-booster
brew install gnu-tar # needed on mac
brew install rg
```

## Install emacs on mac os

### emacs-mac (recommand)

``` bash
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-natural-title-bar --with-native-compilation --with-xwidgets --with-mac-metal # metal is an experimental feature
brew pin emacs-mac # Freeze the version
cp -a $(brew --prefix)/opt/emacs-mac/Emacs.app /Applications
```

### emacs-plus

``` bash
brew tap d12frosted/emacs-plus
brew install --cask d12frosted/emacs-plus/emacs-plus-app
```
