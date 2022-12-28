local config_dir = "~/.config/"

cmd.add_task{
   name="install",
   description="setup my operating system.",
   command=function()
      -- TODO: in the next dosh version I'll add `cmd.ls` command.
      --       update this part later.
      -- local config_dirs = cmd.ls{
      --    parent_dir = ".",
      --    exclude = { "home", "archived" },
      --    file_types = { "directory" },
      -- }

      local config_dirs = { "alacritty", "kitty", "nano", "wezterm" }
      for index = 1, #config_dirs do
         cmd.copy("./" .. config_dirs[index] .. "/", config_dir)
      end

      cmd.copy("./home/*", "~")

      local shell_type

      if env.IS_ZSH then
         shell_type = "zsh"

         if not cmd.exists("~/.oh-my-zsh") then
            cmd.run_url("https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh")
         end
      else
         shell_type = "bash"
      end

      cmd.run("conda init " .. shell_type)

      -- cmd.clone(
      --    "https://github.com/tmux-plugins/tpm",
      --    {
      --       destination="~/.tmux/plugins/tmp",
      --       fetch = true,
      --    }
      -- )
   end
}

cmd.add_task{
   name="install-emacs",
   description="install emacs configuration files.",
   command=function()
   end
}

cmd.add_task{
   name="install-apps",
   description="install my favorite apps.",
   command=function()
      if env.IS_WINDOWS then
         local packages = {
            "Git.Git", "VSCodium.VSCodium", "Discord.Discord", "Valve.Steam"
         }
         cmd.winget_install(packages)
      elseif env.IS_MACOS then
         local packages = {
            "bat", "exa", "fd", "git-delta", "git-lfs", "htop", "llvm", "multimarkdown", "nvm", "openssl", "pass",
            "pre-commit", "ripgrep", "rust-analyzer", "rustup-init", "shellcheck", "tmux", "font-ibm-plex",
            "miktex-console", "miniconda"
         }
         cmd.brew_install(packages, { taps = { "wez/wezterm" }})
      elseif env.IS_LINUX then
         local packages = {
            "git", "ripgrep"
         }
         cmd.apt_install(packages)
      end
   end
}
