bindkey -e  # emacs style key binding

autoload -U select-word-style
select-word-style bash

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history

# this line fixes a problem about emacs tramp:
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

if command -v starship &> /dev/null
then
    export STARSHIP_CONFIG=$HOME/.config/starship/starship.toml
    eval "$(starship init zsh)"
fi

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

  autoload -Uz compinit
  compinit
fi

alias emc="emacsclient -c"
alias ls="ls --color=auto"
alias claude="~/.claude/local/claude"
