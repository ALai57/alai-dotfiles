# doom-config

My doom emacs configuration file.

# Installation/usage
Move your existing .emacs.d/ folder, so you don't clobber it.
Install Doom Emacs. 
`https://github.com/hlissner/doom-emacs`

Clone this repo 
Symlink the files in this folder into your ~/.doom.d/ folder with

``` sh
cp --symbolic-link [doom-config/*] ~/.doom.d/
```

# Useful commands
alias dr='~/.emacs.d/bin/doom sync'

# Installing local packages (e.g. for development)
symlink the package into the `local-packages` directory and add it to config.el
