alias ls="ls -G"
alias xiki="/usr/local/Cellar/ruby/1.9.3-p194/lib/ruby/gems/1.9.1/gems/xiki-0.6.5/bin/xiki"
# alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
# alias emc="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

# Customize to your needs...
export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/local/share/python:/usr/local/Cellar/gettext/0.18.1.1/bin:/usr/local/Cellar/clamav/0.97.4/bin:/usr/local/Cellar/ruby/1.9.3-p194/bin

# export google_appengine
export PYTHONPATH=/usr/local/google_appengine:$PYTHONPATH

# enable virtualenvwrapper
#export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
#source `which virtualenvwrapper.sh`

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
