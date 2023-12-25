# ~/.goedev-env.sh --- GOEDEV terminal environment.

export COLORTERM="truecolor"
export DOSH_ENV="development"
export EDITOR="nano"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export PATH="$HOME/.local/bin:$PATH"
export STARSHIP_CONFIG="$HOME/.config/starship/starship.toml"

test -f "$HOME/.cargo/env" && . "$HOME/.cargo/env"
