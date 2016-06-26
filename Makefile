all: link

link:
	ln -sf ~/dotfiles/.emacs.d ~/.emacs.d;
	ln -sf ~/dotfiles/.zshrc ~/.zshrc;
	ln -sf ~/dotfiles/.vimrc ~/.vimrc;
