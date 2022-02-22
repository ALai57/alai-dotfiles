#!/bin/bash

# Install brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/alai/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"

brew install --cask slack
brew install --cask docker
brew install --cask zoom
brew install postgres
brew install clojure/tools/clojure
brew install wget
brew install bazelisk
brew install awscli
brew install gnupg
brew install jq

# Auth/VPN
brew install saml2aws
brew install --cask aws-vpn-client

## Window management
brew install --cask rectangle
brew install --cask alt-tab
# Amethyst window manager
# Need to also allow amethyst via accessibility options
#brew install --cask amethyst

# Java
brew tap adoptopenjdk/openjdk
brew install --cask adoptopenjdk13

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-native-comp --with-no-titlebar


# Create SSH for GH
#ssh-keygen -t rsa

# Manual 
# - Anaconda
# - Zoom
# - Chrome
# - Globalprotect https://splashfinancial.atlassian.net/wiki/spaces/TW/pages/515932191/Accounts+Setup
# - AWS VPN client
