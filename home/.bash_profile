# ~/.bash_profile --- GKMNGRGN personal bash profile.

# Requirements:
# 1. install starship from starship.rs

alias dosh="bash do.sh"
alias python="python3"
alias ls="ls --color=auto"
alias clip="xclip -selection clipboard"
alias clipw="clip.exe"
alias hm="himalaya"
alias em="emacsclient -nw"

export PATH="$HOME/.local/bin:$HOME/bin:$PATH"               # local
export PATH="$HOME/.cargo/bin:$PATH"                         # rust
export PATH="$HOME/.pub-cache/bin:/usr/lib/dart/bin:$PATH"   # dart
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"         # golang

# editor settings
export EDITOR="emacsclient"

# git supported prompt
eval "$(starship init bash)"

# Dotnet settings
export PATH="$HOME/.dotnet/tools:$PATH"
export DOTNET_ROOT="/snap/dotnet-sdk/current"

# Python settings
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init --path)"
fi

export PATH="$HOME/.poetry/bin:$PATH"

# NVM & Node.js settings
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

. "$HOME/.cargo/env"

# Deno settings
export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# Added by serverless binary installer
export PATH="$HOME/.serverless/bin:$PATH"
