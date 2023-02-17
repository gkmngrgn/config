local config_dir = "~/.config/"

cmd.add_task{
   name="setup",
   description="setup my operating system.",
   required_platforms={"macos"},
   command=function()
      -- check if all required packages are installed
      cmd.brew_install(
         {
            "bat", "exa", "git-delta", "git-lfs", "htop", "nvm", "openssl",
            "rustup-init", "tmux", "font-ibm-plex", "miktex-console", "miniconda",
            "wezterm",
         },
         {
            taps = { "wez/wezterm" }
         }
      )

      -- check if tmux package manager is installed
      cmd.clone(
         "https://github.com/tmux-plugins/tpm",
         {
            destination="~/.tmux/plugins/tpm",
            fetch = true,
         }
      )

      -- copy all configuration files
      local config_dirs = { "alacritty", "kitty", "nano", "wezterm" }
      for index = 1, #config_dirs do
         cmd.copy("./" .. config_dirs[index] .. "/", config_dir)
      end

      cmd.copy("./home/*", "~")

      -- configure shell
      local shell_type

      if env.IS_ZSH then
         shell_type = "zsh"

         if not cmd.exists("~/.oh-my-zsh") then
            cmd.run_url("https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh")
         end
      else
         shell_type = "bash"
      end

      -- configure miniconda
      cmd.run("conda init " .. shell_type)

      -- configure emacs
      cmd.run_url(
         "https://raw.githubusercontent.com/gkmngrgn/emacs.d/main/dosh.lua",
         {
            parameters = "install"
         }
      )
   end
}
