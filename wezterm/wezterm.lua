local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()
local appearance = 'Dark'

-- KEYS
config.keys = {
   {
      key = 'E',
      mods = 'CTRL|SHIFT',
      action = act.PromptInputLine {
         description = 'RENAME TAB TITLE:',
         action = wezterm.action_callback(
            function(window, pane, line)
               if line then
                  window:active_tab():set_title(line)
               end
            end
         ),
      },
   },
}

-- FONT
config.font = wezterm.font {
   family = 'Iosevka Term',
   weight = 'Regular',
}
config.font_size = 18

-- WINDOW
config.initial_cols = 160
config.initial_rows = 42
config.window_decorations = 'TITLE|RESIZE'
config.window_padding = {
   top = 0,
   left = 0,
   right = 0,
   bottom = 0,
}
config.tab_max_width = 30
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.audible_bell = 'Disabled'
config.default_cursor_style = 'BlinkingBlock'
config.bold_brightens_ansi_colors = false

-- THEME
if appearance:find 'Dark' then
   fg = 'white'
   bg = 'black'
else
   fg = 'black'
   bg = 'white'
end

config.colors = {
   foreground = fg,
   background = bg,
   tab_bar = {
      background = bg,
      active_tab = { bg_color = bg, fg_color = fg },
   }
}

return config
