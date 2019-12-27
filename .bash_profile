# ~/.bash_profile --- GKMNGRGN personal bash profile.

# Requirements:
# 1. Install starship with cargo:
#    cargo install starship

alias dosh="bash do.sh"
alias python="python3"
alias ls="ls --color=auto"

export PATH="$HOME/.local/bin:$PATH"                   # local
export PATH="$HOME/.cargo/bin:$PATH"                   # Rust
export PATH="/usr/lib/dart/bin:$PATH"                  # Dart
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"   # Golang

# git supported prompt
eval "$(starship init bash)"

# Pyenv & Python settings
export PATH="$HOME/.pyenv/bin:$PATH"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# NVM & Node.js settings
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
