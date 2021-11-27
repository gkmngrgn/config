# ~/.bash_profile --- GKMNGRGN personal bash profile.
alias ab="abook -f ~/.config/abook/addressbook"
alias bt="bartib -f ~/.config/bartib/records.txt"
alias clip="xclip -selection clipboard"
alias clipw="clip.exe"
alias dosh="bash do.sh"
alias em="emacsclient -nw"
alias hm="himalaya"
alias ls="ls --color=auto"

export PATH="$HOME/.local/bin:$HOME/bin:$PATH"                             # local
export PATH="$HOME/.cargo/bin:$PATH"                                       # rust
export PATH="$HOME/.pub-cache/bin:$HOME/flutter/bin:$PATH"                 # dart & flutter
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"                       # golang
export PATH="$HOME/.local/share/solana/install/active_release/bin:$PATH"   # solana

# EDITOR
export EDITOR="emacsclient"

# DOTNET
export PATH="$HOME/.dotnet/tools:$PATH"
export DOTNET_ROOT="/snap/dotnet-sdk/current"

# PYTHON
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init --path)"
fi

export PATH="$HOME/.poetry/bin:$PATH"

# NVM & NODEJS
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

. "$HOME/.cargo/env"

# DENO
export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# SERVERLESS
export PATH="$HOME/.serverless/bin:$PATH"

# NIX
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi
