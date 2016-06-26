all: emacs

emacs_link:
	ln -sf ~/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el;
	ln -sf ~/dotfiles/.emacs.d/Cask ~/.emacs.d/Cask;

link:
	ln -sf ~/dotfiles/.emacs.d ~/.emacs.d;
	ln -sf ~/dotfiles/.zshrc ~/.zshrc;
	ln -sf ~/dotfiles/.vimrc ~/.vimrc;
