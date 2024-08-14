# Gaming

# Install steam

On Ubuntu

```sh
sudo apt-get install curl

# Download steam deb
sudo dpkg -i steam_latest.deb
```

## Bluetooth?

## Controllers

On linux system, may need to install specific drivers for Xbox One version 1914

It does work with Ubuntu 20.04 LTS with LTS Kernel 5.4 and adding the following things to your /etc/bluetooth/main.conf file:

```
[General]
Privacy = device
JustWorksRepairing = always
Class = 0x000100
FastConnectable = true

[LE]
MinConnectionInterval=7
MaxConnectionInterval=9
ConnectionLatency=0
```

https://www.reddit.com/r/linux_gaming/comments/nztt52/comment/hg627qy/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

https://github.com/atar-axis/xpadneo/issues/295

ERTM
https://www.addictivetips.com/ubuntu-linux-tips/xbox-one-controllers-over-bluetooth-linux/
https://gist.github.com/2E0PGS/0166ffec16b1d86acb4ebeea6871b54e
https://retropie.org.uk/forum/topic/27342/ertm-and-controller-pairing
