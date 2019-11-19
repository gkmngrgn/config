# ~/.bash_profile --- GKMNGRGN personal bash profile.

# Requirements:
# 1. Install starship with cargo:
#    cargo install starship

alias dosh="bash do.sh"
alias python="python3"
alias ls="ls --color=auto"

export PATH="$HOME/.local/bin:$PATH"                   # local
export PATH="$HOME/.cargo/bin:$PATH"                   # rust
export PATH="/usr/lib/dart/bin:$PATH"                  # dart
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"   # go

# git supported prompt
eval "$(starship init bash)"
