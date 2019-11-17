# ~/.bash_profile --- GKMNGRGN personal bash profile.

# Requirements:
# 1. Install starship with cargo:
#    cargo install starship

# colors
BLUE="\\033[1;34m"
GREEN="\\033[1;32m"
NORMAL="\\033[0;39m"
RED="\\033[1;31m"

# use podman instead of docker if it is installed
if command -v podman &>/dev/null
then
  alias docker="podman"
fi
alias dosh="bash do.sh"

# change default python version in debian
alias python="python3"

# my custom settings
if [ -f ~/.config/exercism/exercism_completion.bash ]
then
  source ~/.config/exercism/exercism_completion.bash
fi

export PATH="$HOME/.local/bin:$PATH"                   # local
export PATH="$HOME/.cargo/bin:$PATH"                   # rust
export PATH="/usr/lib/dart/bin:$PATH"                  # dart
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"   # go

# git supported prompt
eval "$(starship init bash)"
