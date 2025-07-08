-- local cmd
local config_dir = "~/.config/"

cmd.add_task {
    name = "setup",
    description = "setup my operating system.",
    required_platforms = { "macos", "linux" },
    command = function()
        -- copy all configuration folders
        local config_dirs = cmd.ls(".", {
            include_files = false,
            excludes = { ".git", "archived", "home" }
        })
        for index = 1, #config_dirs do
            cmd.copy(config_dirs[index], config_dir)
        end

        -- copy home files
        cmd.copy("./home/*", "~")
    end
}

cmd.add_task {
    name = "fix-mosh",
    description = "fix mosh permission issues.",
    required_platforms = { "macos" },
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
