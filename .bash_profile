# ~/.bash_profile --- GKMNGRGN personal bash profile.

# Requirements:
# 1. install starship from starship.rs

alias dosh="bash do.sh"
alias python="python3"
alias ls="ls --color=auto"

export PATH="$HOME/.local/bin:$PATH"                         # local
export PATH="$HOME/.cargo/bin:$PATH"                         # rust
export PATH="$HOME/.pub-cache/bin:/usr/lib/dart/bin:$PATH"   # dart
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"         # golang
export PATH="$HOME/.poetry/bin:$PATH"                        # poetry

# git supported prompt
eval "$(starship init bash)"

# Pyenv & Python settings
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

# NVM & Node.js settings
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

source "$HOME/.cargo/env"
