local wezterm = require 'wezterm'

return {
    initial_cols = 128,
    initial_rows = 32,
    tab_max_width = 30,
    use_fancy_tab_bar = false,
    font_size = 15,
    audible_bell = 'Disabled',
    window_padding = {top = 0, left = 0, right = 0, bottom = 0},
    window_decorations = "TITLE",
    ssh_domains = {
        {name = 'gerudo', remote_address = 'gerudo', username = 'goedev'}
    }
}
