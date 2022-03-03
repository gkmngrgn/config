# ~/.goedev-env.sh --- GOEDEV terminal environment.

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

alias ab="abook -f ~/.config/abook/addressbook"
alias clip="xclip -selection clipboard"
alias clipw="clip.exe"
alias dosh="bash do.sh"
alias em="emacsclient -nw"
alias hm="himalaya"
alias tb="nc yank.gokmengorgen.net 9999"

export PATH="$HOME/.local/bin:$HOME/bin:/usr/local/bin:/usr/bin:/bin"      # default bin folders
export PATH="$HOME/.cargo/bin:$PATH"                                       # rust
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"                       # golang
export PATH="/opt/homebrew/bin/:$PATH"                                     # homebrew

# EDITOR
export EDITOR="nano"
export LSP_USE_PLISTS=true

# PYTHON
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init --path)"
fi

export PATH="$HOME/.poetry/bin:$PATH"

# DENO
export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# SERVERLESS
export PATH="$HOME/.serverless/bin:$PATH"
