#!/bin/bash
# setting .emacs.d
dir=~/.emacs.d; [ ! -e $dir ] && mkdir $dir;
histdir=$dir/cache/savehist; [ ! -e $histdir ] && mkdir -p $histdir;
ln -sf ~/dotfiles/.emacs.d/init.el $dir/init.el;
ln -sf ~/dotfiles/.emacs.d/Cask $dir/Cask;

# package install
cd $dir;
cask install;
cd -;
