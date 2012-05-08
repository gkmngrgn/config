autoload -U promptinit && promptinit
autoload -U colors && colors

PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[green]%}%m %{$fg[yellow]%}%1~ %{$reset_color%}%# "

alias ls="ls -G"
alias emc="emacs -nw"

# emacs style key binding
bindkey -e

# Customize to your needs...
export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/local/share/python

# enable virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
source `which virtualenvwrapper.sh`

# set LANG
export CC='/usr/bin/clang'
export CFLAGS='-Os -w -pipe -march=native -Qunused-arguments -arch x86_64'
export CXX='/usr/bin/clang++'
export CXXFLAGS='-Os -w -pipe -march=native -Qunused-arguments'
export LANG='en_US.UTF-8'
export LC_ALL='en_US.UTF-8'
export LD='/usr/bin/clang'
export LDFLAGS='-arch x86_64'
export MAKEFLAGS='-j4'

# android sdk
export ANDROID_SDK_ROOT=/usr/local/Cellar/android-sdk/r18

# history
## Command history configuration
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data
