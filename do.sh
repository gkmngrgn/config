#!/usr/bin/env bash

CONFIG_DIR="$HOME/.config"
BIN_DIR="$HOME/.local/bin"


print_help() {
    echo "Subcommands:"
    echo "  > install            copy your configuration files to your home folder."
    echo "  > install_cli_apps   install cli apps."
}

install() {
    if [ ! -d "$CONFIG_DIR" ]; then
        mkdir -p $CONFIG_DIR
    fi

    cp -r ./config/* $CONFIG_DIR
    find ./home \
         -maxdepth 1 \
         -iname '.*' \
         -type f \
         -exec cp {} $HOME \;

    if [ ! -e "$CONFIG_DIR/nano/nanorc.d" ]; then
        git clone https://github.com/scopatz/nanorc.git $CONFIG_DIR/nano/nanorc.d
    else
        echo "update nanorc repository..."
        git -C "$CONFIG_DIR/nano/nanorc.d" pull
    fi

    if [ ! -e "$HOME/.oh-my-zsh" ]; then
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    fi

    if [ ! -e "$HOME/.tmux/plugins/tpm" ]; then
        git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
    else
        echo "update tpm repository..."
        git -C "$HOME/.tmux/plugins/tpm" pull
    fi
}

install_cli_apps() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        install_cli_apps_for_deb
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        install_cli_apps_for_mac
    else
        echo -e "Unknown OS."
    fi
}

install_cli_apps_for_mac() {
    brew tap xwmx/taps  # for nb

    brew install -q    \
         bat           \
         cmake         \
         exa           \
         font-ibm-plex \
         git-delta     \
         git-lfs       \
         golang        \
         himalaya      \
         htop          \
         hugo          \
         jq            \
         latexindent   \
         llvm          \
         mactex-no-gui \
         mosh          \
         nb            \
         nano          \
         openssl       \
         pass          \
         pre-commit    \
         ripgrep       \
         rust-analyzer \
         tmux          \
         toilet        \

     # pyenv dependencies
     brew install -q \
         pyenv       \
         readline    \
         sqlite3     \
         xz          \
         zlib

    # install nvm
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
}

install_cli_apps_for_deb() {
    if [ ! -d "$BIN_DIR" ]; then
        mkdir -p $BIN_DIR
    fi

    mkdir -p tmp
    cd tmp

    # install delta
    if hash delta 2>/dev/null; then
        echo -e "delta is already installed. Skipped."
    else
        curl -L -o delta.deb https://github.com/dandavison/delta/releases/download/0.11.3/git-delta_0.11.3_amd64.deb
        sudo dpkg -i delta.deb
    fi

    # install go
    if hash go 2>/dev/null; then
        echo -e "go is already installed. Skipped."
    else
        curl -L -o go.tar.gz https://go.dev/dl/go1.17.5.linux-amd64.tar.gz
        tar -xf go.tar.gz
        mv go $HOME
    fi

    # install jq
    if hash jq 2>/dev/null; then
        echo -e "jq is already installed. Skipped."
    else
        sudo apt install -y jq
    fi

    # install nvm
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash

    # install pyenv
    if hash pyenv 2>/dev/null; then
        echo -e "pyenv is already installed. Skipped."
    else
        curl https://pyenv.run | bash
    fi

    # install ripgrep
    if hash rg 2>/dev/null; then
        echo -e "rg is already installed. Skipped."
    else
        curl -L -o ripgrep.deb https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb
        sudo dpkg -i ripgrep.deb
    fi

    # install rust
    if hash rustc 2>/dev/null; then
        echo -e "rust is already installed. Skipped."
    else
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    fi

    cd ..
    rm -rf tmp
}

if [ -z ${1} ]
then
    print_help
else
    ${@}
fi
