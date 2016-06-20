export LANG=ja_JP.UTF-8

# emacs keybind
bindkey -e

# -------------------------
# setting for cd
# -------------------------
setopt auto_cd

setopt auto_pushd # `cd +<Tab>`
setopt pushd_ignore_dups

# extend specific PATH
# hash -d hoge=/long/path/to/hogehoge 

# -------------------------
# auto-complete
# -------------------------

autoload -U compinit; compinit

setopt auto_list
## 補完候補一覧でファイルの種別をマーク表示
setopt list_types
## 補完候補を詰めて表示
setopt list_packed

setopt auto_resume
setopt auto_param_keys
setopt auto_param_slash

# enable to change candidates by <TAB>
setopt auto_menu
# enable to change candidates by cursors
zstyle ':completion:*:default' menu select=1

# color setting 
# eval `dircolors`
# export ZLS_COLORS=$LS_COLORS
export LS_COLORS='di=36;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;46'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# enable to use extended glob 
# cf: FILENAME GENERATION in `man zshexpn`
setopt extended_glob

# 単語の一部として扱われる文字のセットを指定する
# ここではデフォルトのセットから / を抜いたものとする
# こうすると、 Ctrl-W でカーソル前の1単語を削除したとき、 / までで削除が止まる
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# -------------------------
# setting for git
# -------------------------
autoload -Uz vcs_info

# format
# %b: branch info; %a: action info
zstyle ':vcs_info:*' formats '[git:%b]'
zstyle ':vcs_info:*' actionformats '[git:%b|%a]'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

# -------------------------
# setting for prompt
# -------------------------
autoload colors
PROMPT='%F{magenta}%n@%m %F{white}%d 
%F{yellow}%# '
RPROMPT='%1(v|%F{green}%1v%f|)' # show info of git

# -------------------------
# setting for zsh-history
# -------------------------
HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000

setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt share_history
setopt hist_ignore_space

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end 


DOTS_DIR=~/dotfiles
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
[ -f ${DOTS_DIR}/.zshrc.alias ] && source ${DOTS_DIR}/.zshrc.alias
