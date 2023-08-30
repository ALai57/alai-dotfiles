#!/bin/bash

cd ~/code

# My apps
#git clone git@github.com:ALai57/alai-dotfiles.git
git clone git@github.com:ALai57/kaleidoscope.git
git clone git@github.com:ALai57/kaleidoscope-ui.git
git clone git@github.com:ALai57/walkthroughs.git

# Some basic things
git clone git@github.com:clojure/clojure.git
git clone git@github.com:clojure/clojurescript.git
git clone git://git.savannah.gnu.org/emacs.git

# Interesting libs
git clone git@github.com:mui/material-ui.git
git clone git@github.com:ianstormtaylor/slate.git
git clone git@github.com:udecode/plate.git
git clone git@github.com:metosin/reitit.git
git clone git@github.com:metosin/malli.git
git clone git@github.com:thheller/shadow-cljs.git

# May need to move any existing .emacs.d folders to allow this 
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d


# Freshpaint
git clone --recursive git@github.com:freshpaint-io/perfalytics.git
git clone git@github.com:freshpaint-io/analytics.js.git
git clone git@github.com:freshpaint-io/perfalytics-js.git
