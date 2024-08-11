# Gaming

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
