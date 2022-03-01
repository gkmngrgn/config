#!/usr/bin/env bash

CONFIG_DIR="$HOME/.config"
BIN_DIR="$HOME/.local/bin"


print_help() {
    echo "Subcommands:"
    echo "  > install            copy your configuration files to your home folder."
    echo "  > install_cli_apps   install cli apps."
}

install() {
    install_cli_apps

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

    conda init zsh
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
    brew install -q      \
         MisterTea/et/et \
         bat             \
         cmake           \
         exa             \
         exercism        \
         font-ibm-plex   \
         gh              \
         git-delta       \
         git-lfs         \
         golang          \
         htop            \
         hugo            \
         jq              \
         latexindent     \
         llvm            \
         mactex-no-gui   \
         miniconda       \
         multimarkdown   \
         nano            \
         nvm             \
         openssl         \
         pass            \
         pre-commit      \
         ripgrep         \
         rust-analyzer   \
         shellcheck      \
         tmux            \
         toilet
}


if [ -z ${1} ]
then
    print_help
else
    ${@}
fi
