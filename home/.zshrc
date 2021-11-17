export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

zstyle ':omz:update' mode auto
zstyle ':completion:*:*:git:*' script /usr/share/git/completion/git-completion.zsh

plugins=(git)

source $ZSH/oh-my-zsh.sh
source ~/.bash_profile

bindkey -e   # emacs style key binding

# history
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups         # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history            # share command history data
