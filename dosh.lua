-- local cmd
local config_dir = "~/.config/"

cmd.add_task {
    name = "setup",
    description = "setup my operating system.",
    required_platforms = { "macos" },
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
