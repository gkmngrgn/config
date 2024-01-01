# ~/.goedev-env.sh --- GOEDEV terminal environment.

export CLICOLOR=1
export COLORTERM="truecolor"
export DOSH_ENV="development"
export EDITOR="nano"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export PATH="$HOME/.local/bin:$PATH"
export STARSHIP_CONFIG="$HOME/.config/starship/starship.toml"

export MODULAR_HOME="$HOME/.modular"
export PATH="$MODULAR_HOME/pkg/packages.modular.com_mojo/bin:$PATH"

test -f "$HOME/.cargo/env" && . "$HOME/.cargo/env"
