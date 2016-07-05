DOT=~/dotfiles

all: 
setting:
	inst_brew
	inst_from_brew
	set_zsh
	set_aspell
	set_git
	set_vim
	set_emacs

inst_software: inst_brew inst_from_brew inst_emacs

inst_cask:
	bash Install/install_cask.sh;
	echo 'export PATH="$HOME/.cask/bin:$PATH"' >> .zshrc.local;
	cask upgrade

inst_brew:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)";

inst_from_brew:
	brew install zsh zsh-completions;
	brew install gcc make ncurses-devel; # for emacs 
	brew install wget automake autoconf
	brew install markdown;
	brew install coreutils;

inst_emacs:
	cd /usr/local; 
	wget http://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-24.5.tar.gz;
	tar xvf emacs-24.5.tar.gz;
	cd emacs-24.5;
	./configure;
	sudo make;
	sudo make install;
	cd ${DOT};

set_emacs:
	# bash Install/set_emacs.sh;
	# setting .emacs.d
	dir=~/.emacs.d; [ ! -e $dir ] && mkdir $dir;
	histdir=$dir/cache/savehist; [ ! -e $histdir ] && mkdir -p $histdir;
	ln -sf ~/dotfiles/.emacs.d/init.el $dir/init.el;
	ln -sf ~/dotfiles/.emacs.d/Cask $dir/Cask;

	# package install
	cd $dir;
	cask install;
	cd -;
set_vim:
	ln -sf ${DOT}/.vimrc ${HOME}/;

set_zsh:
	ln -sf ${DOT}/.zshrc ${HOME}/;
	[ -f .zshrc_`uname` ] && cp .zshrc_`uname` ${HOME}/.zshrc.local;

set_git:
	brew install git;
	echo 'export PATH="/usr/local/Cellar/git/2.9.0/bin:$PATH"' >> .zshrc.local;

set_aspell:
	brew install aspell;
	echo "lang en_US" > ~/.aspell.conf;
