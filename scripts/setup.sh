#!/bin/bash

sudo add-apt-repository -y ppa:lutris-team/lutris
sudo add-apt-repository -y ppa:regolith-linux/release
sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
# Required to use the latest mesa (graphics) drivers
# Not sure exactly the interactino between this and proprietary nvidia drivers
#  but make sure you're running Nvidia's own proprietary drivers
#  they are significantly better!
sudo add-apt-repository ppa:kisak/kisak-mesa
                
# Docker install
function install_docker_creds(){
	# Only need to run this once
	curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
	echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  		$(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
}

#install_docker_creds()

# Manual 
# - Anaconda
# - Steam
# - Zoom
# - Chrome

sudo apt update

sudo apt-get install -y git vim
sudo apt-get install -y postgresql awscli
sudo apt-get install -y docker-ce docker-ce-cli containerd.io
sudo apt-get install -y gnome-tweak-tool
sudo apt-get install -y regolith-desktop-standard
sudo apt-get install -y lutris
# For conda
sudo apt-get install -y libgl1-mesa-glx libegl1-mesa libxrandr2 libxrandr2 libxss1 libxcursor1 libxcomposite1 libasound2 libxi6 libxtst6k

# Depending on the Nvida-driver - a tool for measuring GPU
#sudo apt-get install nvidia-utils-495

## For Emacs native comp
sudo apt-get install autoconf make texinfo build-essential install install-info info libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses5-dev libxpm-dev automake autoconf libgnutls28-dev
sudo apt-get install -y gcc-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev libgccjit-9-dev
sudo apt-get install -y openjdk-17-jre	openjdk-16-jre

# Docker 
sudo usermod -aG docker $USER
