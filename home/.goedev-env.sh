# ~/.goedev-env.sh --- GOEDEV terminal environment.

export COLORTERM="truecolor"
export DOSH_ENV="development"
export ALTERNATE_EDITOR="nano"
export EDITOR="emacsclient -t"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export PATH="$HOME/.local/bin:$PATH"

alias edit='eval "$EDITOR"'

PATH_HOMEBREW="/opt/homebrew/bin/brew"
if command -v "$PATH_HOMEBREW" &> /dev/null
then
   eval "$($PATH_HOMEBREW shellenv)"
fi

if command -v pyenv &> /dev/null
then
    eval "$(pyenv init -)"
fi
