#!/bin/bash

# Install brew
#/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
#echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/alai/.zprofile
#eval "$(/opt/homebrew/bin/brew shellenv)"

# brew install --cask slack
# brew install --cask zoom
# brew install --cask docker
brew install postgresql@14
brew services start postgresql@14
brew install libpq
echo 'export PATH="/opt/homebrew/opt/libpq/bin:$PATH"' >> ~/.zshrc

brew install clojure/tools/clojure
brew install wget
brew install awscli
brew install gnupg
brew install jq

## Window management
# brew install --cask rectangle

# Java
brew install openjdk
sudo ln -sfn /opt/homebrew/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@30 --with-native-comp --with-xwidgets
# Create a Mac alias so it can be found by launcher https://apple.stackexchange.com/a/32495

# Other
brew install tfenv
brew install leiningen
brew install node
brew install ripgrep

# Create SSH for GH
#ssh-keygen -t rsa


# Task
brew install go-task/tap/go-task

# Go linter
# binary will be $(go env GOPATH)/bin/golangci-lint
curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.54.2

golangci-lint --version

# NVM
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash

# Direnv - directory-specific environment variables
# https://github.com/direnv/direnv
brew install direnv
eval "$(direnv hook zsh)"

# Manual
# - Notion.so desktop app https://www.notion.so/desktop
# - Displaylink manager  https://www.synaptics.com/products/displaylink-graphics/downloads/macos-connectivity-1.9?filetype=exe
# - Anaconda
# - Chrome
#   - Reset Chrome keybindings https://superuser.com/questions/1293474/osx-chrome-use-ctrl-key-based-shortcuts-instead-of-command-key-based
# - Docker desktop https://www.docker.com/products/docker-desktop/
# - https://github.com/waydabber/BetterDisplay/releases Setup better display for better monitor management
# - Set key repeat speed https://support.apple.com/guide/mac-help/set-how-quickly-a-key-repeats-mchl0311bdb4/mac#:~:text=On%20your%20Mac%2C%20choose%20Apple,may%20need%20to%20scroll%20down.)&text=Drag%20the%20%E2%80%9CDelay%20until%20repeat,set%20how%20fast%20characters%20repeat.
# - Set keybindings/shortcuts for Copy Paste Cut
# - Symlink ZProfile ln -s /Users/alai/spl/alai-dotfiles/profiles/.zprofile.splash /Users/alai/.zprofile
# - Rebind mission control (show all windows) if desired: Keyboard -> Keyboard Shortcuts -> Mission Control

# Navigation
# - Rebind CAPS LOCK to control https://appleinsider.com/inside/macos/tips/how-to-remap-caps-lock-control-option-command-keys-in-macos

## TODO:
## plantuml
## visual studio code
