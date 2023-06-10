local wezterm = require 'wezterm'

function add_colors_for_appearance(settings, appearance)
  local fg, bg

  if appearance:find 'Dark' then
    fg = 'white'
    bg = 'black'
  else
    fg = 'black'
    bg = 'white'
  end

  settings.colors = {
    foreground = fg,
    background = bg,
    tab_bar = {
      background = bg,
      active_tab = { bg_color = bg, fg_color = fg },
    }
  }
end

function add_display_config(settings, display_name)
  if display_name == 'internal' then
    settings.font_size = 15.5
    settings.window_padding = { top = 5, left = 0, right = 0, bottom = 0 }
  elseif display_name == 'external' then
    settings.font_size = 15
    settings.line_height = 0.9
    settings.window_padding = { top = 7, left = 0, right = 0, bottom = 0 }
  end
end

wezterm_config = {
  font = wezterm.font {
    family = 'Jetbrains Mono',
    stretch = 'Expanded',
    weight = 'Regular',
  },
  initial_cols = 160,
  initial_rows = 42,
  tab_max_width = 30,
  tab_bar_at_bottom = true,
  use_fancy_tab_bar = false,
  audible_bell = 'Disabled',
  window_decorations = 'TITLE|RESIZE',
  default_cursor_style = 'BlinkingBlock',
  bold_brightens_ansi_colors = false,
}

add_colors_for_appearance(wezterm_config, 'Dark')
add_display_config(wezterm_config, 'internal')

return wezterm_config
