# ~/.goedev-env.sh --- GOEDEV terminal environment.

export COLORTERM="truecolor"
export DOSH_ENV="development"
export EDITOR="emacsclient -t -a nano"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

if command -v /opt/homebrew/bin/brew &> /dev/null
then
   eval "$(/opt/homebrew/bin/brew shellenv)"
fi
