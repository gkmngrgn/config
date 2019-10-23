# ~/.bash_profile --- GKMNGRGN personal bash profile.

# Requirements:
# 1. Install git bash prompt:
#    git clone https://github.com/magicmonty/bash-git-prompt.git ~/.bash-git-prompt --depth=1

# if running bash
# if [ -n "$BASH_VERSION" ]
# then
#  # include .bashrc if it exists
#  if [ -f "$HOME/.bashrc" ]
#  then
#    . "$HOME/.bashrc"
#  fi
# fi

# colors
BLUE="\\033[1;34m"
GREEN="\\033[1;32m"
NORMAL="\\033[0;39m"
RED="\\033[1;31m"

# git prompt
PATH_GIT_PROMPT="$HOME/.bash-git-prompt/gitprompt.sh"
if [ -f $PATH_GIT_PROMPT ]
then
  GIT_PROMPT_THEME="Minimal"
  source $PATH_GIT_PROMPT
else
  echo -e "${RED}The extension gitprompt is missing.${NORMAL}"
fi

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
