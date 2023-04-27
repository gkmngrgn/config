local wezterm = require 'wezterm'

function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function colors_for_appearance(appearance)
  local fg, bg

  if appearance:find 'Dark' then
    fg = 'white'
    bg = 'black'
  else
    fg = 'black'
    bg = 'white'
  end

  return {
    foreground = fg,
    background = bg,
    tab_bar = {
      background = bg,
      active_tab = { bg_color = bg, fg_color = fg },
    }
  }
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
  tab_bar_at_bottom = true,
  use_fancy_tab_bar = false,
  window_padding = { top = 0, left = 0, right = 0, bottom = 0 },
  audible_bell = 'Disabled',
  window_decorations = 'TITLE|RESIZE',
  default_cursor_style = 'BlinkingBlock',
  bold_brightens_ansi_colors = false
}
