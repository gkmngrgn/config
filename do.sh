#!/usr/bin/env bash

CONFIG_DIR="$HOME/.config"

print_help() {
    echo "Subcommands:"
    echo "  > install   copy your configuration files to your home folder."
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
}

if [ -z ${1} ]
then
    print_help
else
    ${@}
fi
