# Check for an interactive session
[ -z "$PS1" ] && return

export EDITOR="nano"

if [ -f /home ]; then
    alias ls='ls --color=auto'
    export SDL_AUDIODRIVER="pulse"
else
    source /usr/local/Cellar/coreutils/8.12/aliases
    alias ls='ls -G'
fi

if [ "$TERM" != "dumb" ]; then
    eval `dircolors ~/.dir_colors`
fi

PS1='\[\033[0;37m\][\u \[\033[1m\]~ \[\033[0;36m\]\h] \[\033[0;32m\][\W]> \[\033[00m\]'

alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias emc="emacs -nw"
