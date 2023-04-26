local wezterm = require 'wezterm'

function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function colors_for_appearance(appearance)
  if appearance:find 'Dark' then
    return {}
  else
    return {
      foreground = 'black',
      background = 'white',
      ansi = { 'black', 'maroon', 'green', 'olive', 'navy', 'purple', 'teal', 'silver' },
      tab_bar = {
        background = '#464646',
        active_tab = { bg_color = '#f7f7f7', fg_color = '#464646' },
        inactive_tab = { bg_color = '#747474', fg_color = '#f7f7f7' },
        new_tab = { bg_color = '#525252', fg_color = '#f7f7f7' },
      },
    }
  end
end

return {
  font = wezterm.font {
    family = 'Jetbrains Mono',
    stretch = 'Expanded',
    weight = 'Regular',
  },
  font_size = 15,
  colors = colors_for_appearance(get_appearance()),
  initial_cols = 160,
  initial_rows = 42,
  tab_max_width = 30,
  use_fancy_tab_bar = false,
  window_padding = { top = 0, left = 0, right = 0, bottom = 0 },
  audible_bell = 'Disabled',
  window_decorations = 'TITLE|RESIZE',
  default_cursor_style = 'BlinkingBlock',
  bold_brightens_ansi_colors = false
}
