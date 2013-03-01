alias ls="ls --color"
alias n="nano"
alias emc="emacsclient -c"
alias emt="emacsclient -t"

# Customize to your needs...
export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/bin/vendor_perl

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

# set default editor
export EDITOR='emacsclient -t'

# virtualenvwrapper
source `which virtualenvwrapper.sh`

# java fonts
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
