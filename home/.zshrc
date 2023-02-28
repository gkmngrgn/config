source ~/.goedev-env.sh

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

zstyle ':omz:update' mode auto
zstyle ':completion:*:*:git:*' script /usr/share/git/completion/git-completion.zsh

plugins=(
    cp
    git
    git-lfs
    man
    pass
    ripgrep
    rust
    tmux
)

source $ZSH/oh-my-zsh.sh

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

# this line fixes a problem about emacs tramp:
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
