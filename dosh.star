CONFIG_DIR = path_home(".config")


def cmd_setup_my_os():
    """place my config files."""

    copy("./config/*", CONFIG_DIR)
    copy("./home/.*", path_home())

    if IS_ZSH:
        if not exists(path_home(".oh-my-zsh")):
            eval_url("https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh")

        if exists_command("conda"):
            eval("conda init zsh")

    clone("https://github.com/tmux-plugins/tpm", dst=path_home(".tmux/plugins/tmp"), fetch=True)


def cmd_install_cli_apps():
    """install my favorite apps."""

    if IS_WINDOWS:
        packages = ["Git.Git", "VSCodium.VSCodium", "Discord.Discord", "Valve.Steam"]
        winget_install(packages)

    elif IS_MACOS:
        packages = [
            "MisterTea/et/et", "bat", "clojure", "cmake", "deno", "exa", "exercism", "fd",
            "git-delta", "git-lfs", "golang", "helix", "htop", "hugo", "jq", "llvm",
            "multimarkdown", "openssl", "pass", "pre-commit", "ripgrep", "rustup-init",
            "rust-analyzer", "shellcheck", "tmux", "font-ibm-plex", "miktex-console", "miniconda"
        ]
        taps=["helix-editor/helix"]
        brew_install(packages, cask=True, taps=taps)

    elif IS_LINUX:
        packages = ["git", "ripgrep"]
        apt_install(packages)

    if not IS_WINDOWS and not exists_command("nvm"):
        eval_url("https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh")
