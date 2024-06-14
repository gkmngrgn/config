export CLICOLOR=1
export COLORTERM="truecolor"
export DOSH_ENV="development"
export EDITOR="nano"
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=10000
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export PATH="$HOME/.local/bin:$PATH"
export SAVEHIST=10000
export HOMEBREW_NO_ENV_HINTS=1

test -f "$HOME/.cargo/env" && . "$HOME/.cargo/env"
