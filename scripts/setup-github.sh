#!/bin/bash

cd ~/spl

# My apps
git clone git@github.com:ALai57/alai-dotfiles.git
git clone git@github.com:ALai57/andrewslai.git
git clone git@github.com:ALai57/andrewslai-frontend.git
git clone git@github.com:ALai57/walkthroughs.git
git clone git@github.com:ALai57/figwheel-alt.git
git clone git@github.com:ALai57/fourclojure.el.git

# Some basic things
git clone git@github.com:clojure/clojure.git
git clone git@github.com:clojure/clojurescript.git
git clone git@github.com:bhauman/figwheel-main.git
git clone git@github.com:koekeishiya/limelight.git
git clone git://git.savannah.gnu.org/emacs.git

# Splash
git clone git@github.com:SplashFinancial/Website.git
git clone git@github.com:SplashFinancial/stonehenge.git
git clone git@github.com:SplashFinancial/ApplicationProcessor.git
git clone git@github.com:SplashFinancial/RatesApi.git

# May need to move any existing .emacs.d folders to allow this 
#git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
