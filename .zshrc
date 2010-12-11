# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
# End of lines configured by zsh-newuser-install

autoload -U colors
colors

autoload -U compinit
compinit

bindkey -e emacs
bindkey ' ' magic-space

bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line
bindkey "^[OH" beginning-of-line
bindkey "^[OF" end-of-line
bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^?" backward-delete-char
bindkey "^[[3~" delete-char

setopt NO_BEEP
setopt CORRECT_ALL
setopt AUTO_LIST
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT
setopt INTERACTIVE_COMMENTS
setopt NONOMATCH
setopt NOPROMPT_SP

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh_cache

export PS1="%{$fg[white]%}[%n ~ %{$fg[cyan]%}%m]%{$fg[green]%} [%~%{$fg[green]%}]>%{$reset_color%} "

alias rm="rm -i"
alias emc="emacs -nw"

export PATH="/usr/local/bin:$PATH"
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export LC_ALL=en_US.UTF-8