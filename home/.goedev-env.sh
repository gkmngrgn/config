# ~/.goedev-env.sh --- GOEDEV terminal environment.

export COLORTERM="truecolor"
export DOSH_ENV="development"
export ALTERNATE_EDITOR="nano"
export EDITOR="emacsclient -t"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export PATH="$HOME/.local/bin:$PATH"

alias edit='eval "$EDITOR"'

if command -v /opt/homebrew/bin/brew &> /dev/null
then
   eval "$(/opt/homebrew/bin/brew shellenv)"
fi
