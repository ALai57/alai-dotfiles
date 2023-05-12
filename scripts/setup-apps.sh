#!/bin/bash

cd ~/
mkdir -p bin
cd bin

# Leiningen
function install_lein() {
	wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
	chmod +x ~/bin/lein
}

function install_doom(){
	doom install
	cp -r ~/.doom.d ~/.doom.d.old
	# Needs to point to the correct location for the `dotfiles` repo
	sudo ln -s ~/spl/alai-dotfiles/.doom.d ~/.doom.d
	doom sync
}

function install_emacs(){
	# https://www.emacswiki.org/emacs/EmacsForMacOS
	osascript -e "tell application \"Finder\" to make alias file to (POSIX file \"/opt/homebrew/Cellar/emacs-plus@30/30.0.50/Emacs.app\") at POSIX file \"$HOME/Applications\""
}

#install_emacs
install_lein
install_doom
