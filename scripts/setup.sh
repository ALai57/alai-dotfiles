#!/bin/bash

# Install brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/alai/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"

brew install --cask slack
brew install --cask docker
brew install --cask zoom
brew install postgres

# Java
brew tap adoptopenjdk/openjdk
brew install --cask adoptopenjdk13

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-native-comp --with-no-titlebar

# Amethyst window manager
# Need to also allow amethyst via accessibility options
brew install --cask amethyst

# Create SSH for GH
#ssh-keygen -t rsa

# Manual 
# - Anaconda
# - Zoom
# - Chrome
