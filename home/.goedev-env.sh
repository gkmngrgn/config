# ~/.goedev-env.sh --- GOEDEV terminal environment.

export DOSH_ENV="development"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export COLORTERM="truecolor"

alias gerudo="et gerudo -c tmux"

export PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin"   # default bin folders
export PATH="$HOME/.local/bin:$HOME/bin:$PATH"               # default bin folders
export PATH="$HOME/.cargo/bin:$PATH"                         # rust
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"         # golang
export PATH="/opt/homebrew/bin/:$PATH"                       # homebrew

# EDITOR
export EDITOR="nano"
export LSP_USE_PLISTS=true

# PYTHON
export PATH="$HOME/.poetry/bin:$PATH"

# DENO
export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# NODE
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# LINUX BREW
LINUXBREW=/home/linuxbrew/.linuxbrew/bin/brew
if test -f "$LINUXBREW"; then
    eval "$($LINUXBREW shellenv)"
fi
