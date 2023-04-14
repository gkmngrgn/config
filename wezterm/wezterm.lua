local wezterm = require 'wezterm'

return {
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
    window_padding = {top = 0, left = 0, right = 0, bottom = 0},
    audible_bell = 'Disabled',
    window_decorations = 'TITLE',
}
