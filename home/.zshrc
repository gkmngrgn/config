autoload -U promptinit && promptinit
autoload -U compinit && compinit
autoload -U colors && colors

# Emulate tcsh's backward-delete-word
tcsh-backward-delete-word () {
    #local WORDCHARS="${WORDCHARS:s#/#}"
    #local WORDCHARS='*?_[]~\/!#$%^<>|`@#$%^*()+?'
    local WORDCHARS="${WORDCHARS:s#/#}"
    zle backward-delete-word
}

zle -N tcsh-backward-delete-word

# emacs style key binding
bindkey -e

# for escape backspace (delete a word) behavior similar to tcsh
bindkey '\e^?' tcsh-backward-delete-word

PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[green]%}%m %{$fg[yellow]%}%1~ %{$reset_color%}%# "

# history
## Command history configuration
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data

source ~/.bash_profile
zstyle ':completion:*:*:git:*' script /usr/share/git/completion/git-completion.zsh
