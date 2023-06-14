#!/bin/bash

# Install brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/alai/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"

# brew install --cask slack
# brew install --cask zoom
# brew install --cask docker
brew install postgresql@14
brew services start postgresql@14
brew install libpq
echo 'export PATH="/opt/homebrew/opt/libpq/bin:$PATH"' >> ~/.zshrc

brew install clojure/tools/clojure
brew install wget
brew install bazelisk
brew install awscli
brew install gnupg
brew install jq

# Auth/VPN
brew install saml2aws
sudo softwareupdate --install-rosetta
brew install --cask aws-vpn-client

## Window management
brew install --cask rectangle
brew install --cask alt-tab
# Amethyst window manager
# Need to also allow amethyst via accessibility options
#brew install --cask amethyst

# Java
#brew tap adoptopenjdk/openjdk
#brew install --cask adoptopenjdk18
brew install openjdk
sudo ln -sfn /opt/homebrew/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk

# Emacs
#brew tap d12frosted/emacs-plus
#brew install emacs-plus@30 --with-native-comp --with-xwidgets
# Create a Mac alias so it can be found by launcher https://apple.stackexchange.com/a/32495

# Other
brew install tfenv
brew install leiningen
brew install node
brew install ripgrep

# Create SSH for GH
#ssh-keygen -t rsa

# Manual 
# - Anaconda
# - Chrome
#   - Reset Chrome keybindings https://superuser.com/questions/1293474/osx-chrome-use-ctrl-key-based-shortcuts-instead-of-command-key-based
# - Globalprotect https://splashfinancial.atlassian.net/wiki/spaces/TW/pages/515932191/Accounts+Setup
# - Docker desktop https://www.docker.com/products/docker-desktop/
# - https://github.com/waydabber/BetterDisplay/releases Setup better display for better monitor management
# - Set key repeat speed https://support.apple.com/guide/mac-help/set-how-quickly-a-key-repeats-mchl0311bdb4/mac#:~:text=On%20your%20Mac%2C%20choose%20Apple,may%20need%20to%20scroll%20down.)&text=Drag%20the%20%E2%80%9CDelay%20until%20repeat,set%20how%20fast%20characters%20repeat.
# - Set keybindings/shortcuts for Copy Paste Cut
# - Symlink ZProfile ln -s /Users/alai/spl/alai-dotfiles/profiles/.zprofile.splash /Users/alai/.zprofile
# - Rebind mission control if desired: Keyboard -> Keyboard Shortcuts -> Launchpad

# Navigation
# - Rebind CAPS LOCK to control https://appleinsider.com/inside/macos/tips/how-to-remap-caps-lock-control-option-command-keys-in-macos

# AWS SSO
# Add config entries to ~/.aws/config
#
# [profile PROFILE_NAME]
# sso_start_url = START-URL-HERE
# sso_region = us-east-1
# sso_account_id = ACCOUNT-ID-HERE
# sso_role_name = tribe_sso_permission_set
# region = us-east-1

## TODO:
## plantuml
## visual studio code
## PHP
