#!/usr/bin/env bash

CONFIG_DIR="$HOME/.config"
BIN_DIR="$HOME/.local/bin"


print_help() {
    echo "Subcommands:"
    echo "  > install            copy your configuration files to your home folder"
    echo "  > install_cli_apps   install cli apps"
    echo "  > repositories       list all repositories"
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

    conda init zsh
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
         font-ibm-plex   \
         gh              \
         git-delta       \
         git-lfs         \
         golang          \
         helix           \
         htop            \
         hugo            \
         jq              \
         llvm            \
         miktex-console  \
         miniconda       \
         multimarkdown   \
         openssl         \
         pass            \
         pre-commit      \
         ripgrep         \
         rust-analyzer   \
         shellcheck      \
         tmux            \
         toilet

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
}

repositories() {
    for repo in $(curl -X GET "https://git.gokmengorgen.net/api/v1/repos/search?uid=goedev&limit=10000" \
                       -H "accept: application/json" \
                       -H "Authorization: token 78bf524da43ee281157150b7173d785d125fdd83" \
                       -H "Content-Type: application/json" \
                       -s | jq -c ".data[] | {ssh_url,name}"); do
        ssh_url=$(echo $repo | jq -r ".ssh_url")
        name=$(echo $repo | jq -r ".name")
        repo_dir="$HOME/Workspace/goedev/$name"

        if [ -d $repo_dir ]; then
            echo "Repo $name is syncing..."
            git -C $repo_dir pull
        else
            echo "Repo $name is cloning..."
            git clone $ssh_url $repo_dir
        fi
        echo ""
    done
}


if [ -z ${1} ]
then
    print_help
else
    ${@}
fi
