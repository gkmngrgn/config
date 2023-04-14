local config_dir = "~/.config/"

cmd.add_task {
    name = "setup",
    description = "setup my operating system.",
    required_platforms = {"macos"},
    -- command_options = {
    --     setup_type = {
    --         type = "string",
    --         options = {"all", "emacs", "python", "shell", "tmux"},
    --         default = "all"
    --     }
    -- },
    command = function(arg)
        arg = arg or "all"

        if arg == "all" or arg == "shell" then
            -- check if all required packages are installed
            cmd.brew_install({
                "bat", "exa", "git-lfs", "htop", "nvm", "pyenv", "openssl",
                "font-jetbrains-mono", "wezterm"
            })

            -- copy all configuration files
            local config_dirs = cmd.ls(".", {
                include_files = false,
                excludes = {".git", "archived", "home"}
            })
            for index = 1, #config_dirs do
                cmd.copy(config_dirs[index], config_dir)
            end

            cmd.copy("./home/*", "~")

            if env.IS_ZSH and not cmd.exists("~/.oh-my-zsh") then
                cmd.run_url(
                    "https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh")
            end
        end

        if arg == "tmux" or arg == "all" then
            cmd.brew_install({"tmux"})
            cmd.clone("https://github.com/tmux-plugins/tpm",
                      {destination = "~/.tmux/plugins/tpm", fetch = true})
        end

        if arg == "python" or arg == "all" then
            cmd.brew_install({"pyenv"})
        end

        -- if arg == "emacs" or arg == "all" then
        --     cmd.run_url(
        --         "https://raw.githubusercontent.com/gkmngrgn/emacs.d/main/dosh.lua",
        --         {parameters = "install"})
        -- end
    end
}

cmd.add_task {
    name = "fix-mosh",
    description = "fix mosh permission issues.",
    required_platforms = {"macos"},
    command = function(arg)
        local firepower = "sudo /usr/libexec/ApplicationFirewall/socketfilterfw"

        -- temporarily shut firewall off
        cmd.run(firepower .. " --setglobalstate off")

        -- add symlinked location to firewall
        cmd.run(firepower .. " --add $(which mosh-server)")
        cmd.run(firepower .. " --unblockapp $(which mosh-server)")

        -- add homebrew location to firewall
        cmd.run(firepower .. " --add $(realpath $(which mosh-server))")
        cmd.run(firepower .. " --unblockapp $(realpath $(which mosh-server))")

        -- re-enable firewall
        cmd.run(firepower .. " --setglobalstate on")
    end
}
