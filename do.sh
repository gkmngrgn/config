#!/usr/bin/env bash

CONFIG_DIR="$HOME/.config"
BIN_DIR="$HOME/.local/bin"


print_help() {
    echo "Subcommands:"
    echo "  > install            copy your configuration files to your home folder"
    echo "  > install_cli_apps   install cli apps"
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

    if [ ! -e "$HOME/.oh-my-zsh" ]; then
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    fi

    if [ ! -e "$HOME/.tmux/plugins/tpm" ]; then
        git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
    else
        echo "update tpm repository..."
        git -C "$HOME/.tmux/plugins/tpm" pull
    fi

    if command -v conda &> /dev/null
    then
        conda init zsh
    fi
}

install_cli_apps() {
    brew tap helix-editor/helix

    brew install -q      \
         MisterTea/et/et \
         bat             \
         clojure         \
         cmake           \
         deno            \
         exa             \
         exercism        \
         fd              \
         git-delta       \
         git-lfs         \
         golang          \
         helix           \
         htop            \
         hugo            \
         jq              \
         llvm            \
         multimarkdown   \
         openssl         \
         pass            \
         pre-commit      \
         ripgrep         \
         rustup-init     \
         rust-analyzer   \
         shellcheck      \
         tmux

    if [[ "$OSTYPE" == "darwin"* ]]; then
        brew install -q     \
             font-ibm-plex  \
             miktex-console \
             miniconda
    fi

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
}


if [ -z ${1} ]
then
    print_help
else
    ${@}
fi
