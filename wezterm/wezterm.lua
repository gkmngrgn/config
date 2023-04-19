local wezterm = require 'wezterm'

function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'One Dark (Gogh)'
  else
    return 'One Light (Gogh)'
  end
end

return {
  color_scheme = scheme_for_appearance(get_appearance()),
  font = wezterm.font {
    family = 'Jetbrains Mono',
    stretch = 'Expanded',
    weight = 'Regular',
  },
  font_size = 15,
  initial_cols = 160,
  initial_rows = 42,
  tab_max_width = 30,
  use_fancy_tab_bar = false,
  window_padding = { top = 0, left = 0, right = 0, bottom = 0 },
  audible_bell = 'Disabled',
  window_decorations = 'TITLE|RESIZE',
  default_cursor_style = 'BlinkingBar',
  cursor_thickness = '0.1cell'
}
