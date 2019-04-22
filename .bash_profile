# if running bash
if [ -n "$BASH_VERSION" ]; then
  # include .bashrc if it exists
  if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
  fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
  PATH="$HOME/.local/bin:$PATH"
fi

# my custom settings
PATH="$PATH:/usr/lib/dart/bin"

if [ -f ~/.config/exercism/exercism_completion.bash ]; then
  source ~/.config/exercism/exercism_completion.bash
fi

if [ -d "$HOME/.local/go/bin" ] ; then
  PATH="$PATH:$HOME/.local/go/bin"
fi
