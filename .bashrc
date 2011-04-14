# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias emc="emacs -nw"

export SDL_AUDIODRIVER="pulse"
export EDITOR="vim"

PS1='\[\033[0;37m\][\u \[\033[1m\]~ \[\033[0;36m\]\h] \[\033[0;32m\][\W]> \[\033[00m\]'
eval `dircolors -b ~/.dir_colors`
