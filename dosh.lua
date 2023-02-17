local config_dir = "~/.config/"

cmd.add_task {
    name = "setup",
    description = "setup my operating system.",
    required_platforms = {"macos"},
    command = function(arg)
        arg = arg or "all"
        local shell_type = env.IS_ZSH and "zsh" or "bash"

        -- check if all required packages are installed
        cmd.brew_install({
            "bat", "exa", "git-delta", "git-lfs", "htop", "nvm", "openssl",
            "font-ibm-plex", "wezterm"
        }, {taps = {"wez/wezterm"}})

        -- copy all configuration files
        local config_dirs = {"alacritty", "kitty", "nano", "wezterm"}
        for index = 1, #config_dirs do
            cmd.copy("./" .. config_dirs[index] .. "/", config_dir)
        end

        cmd.copy("./home/*", "~")

        if arg == "tmux" or arg == "all" then
            cmd.brew_install({"tmux"})
            cmd.clone("https://github.com/tmux-plugins/tpm",
                      {destination = "~/.tmux/plugins/tpm", fetch = true})
        end

        if arg == "shell" or arg == "all" then
            if env.IS_ZSH then
                if not cmd.exists("~/.oh-my-zsh") then
                    cmd.run_url(
                        "https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh")
                end
            end
        end

        if arg == "python" or arg == "all" then
            cmd.brew_install({"mambaforge"})
            cmd.run("mamba init " .. shell_type)
        end

        if arg == "emacs" or arg == "all" then
            cmd.run_url(
                "https://raw.githubusercontent.com/gkmngrgn/emacs.d/main/dosh.lua",
                {parameters = "install"})
        end
    end
}
