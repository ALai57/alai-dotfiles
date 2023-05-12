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

#install_emacs
install_lein
install_doom
