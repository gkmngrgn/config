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

wezterm_config = {
    font = wezterm.font {
        family = 'Iosevka Term',
        weight = 'Regular',
    },
    font_size = 16,
    window_padding = { top = 0, left = 0, right = 0, bottom = 0 },
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

return wezterm_config
