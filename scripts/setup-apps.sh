#!/bin/bash

cd ~/
mkdir -p bin
cd bin

# Leiningen
function install_lein() {
	wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
}

# Clojure
function install_clojure() {
	curl -O https://download.clojure.org/install/linux-install-1.10.3.1029.sh
	chmod +x linux-install-1.10.3.1029.sh
	sudo ./linux-install-1.10.3.1029.sh
}

function install_emacs() {
	# Several other deps need to be install before running this (in the setup.sh script)
	cd ~/dev/emacs
	sudo CC="/usr/bin/gcc-10" ./autogen.sh && ./configure --with-native-compilation --with-mailutils
	sudo make -j 4 && make install
}

function install_doom(){
	doom install
	cp -r ~/.doom.d ~/.doom.d.old
	# Needs to point to the correct location for the `dotfiles` repo
	sudo ln -s ~/dev/alai-dotfiles/.doom.d ~/.doom.d
	doom sync
}

function install_discord(){
	sudo snap install discord
}

#install_emacs
install_lein
