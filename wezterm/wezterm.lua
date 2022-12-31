local wezterm = require 'wezterm'

return {
   color_scheme='Adventure',
   initial_cols = 130,
   initial_rows = 40,
   tab_bar_at_bottom = true,
   tab_max_width = 30,
   use_fancy_tab_bar = false,
   font_size = 15,
   window_decorations = "TITLE",
   window_padding = {
      top = 0,
      right = 0,
      bottom = 0,
      left = 0,
   },
   audible_bell = 'Disabled',
   ssh_domains = {
      {
         name = 'gerudo',
         remote_address = 'gerudo',
         username = 'goedev',
      },
   },
}
