local colors = {
    base_0 = '#ffffff',
    base_1 = '#f2f2f2',
    base_2 = '#eaeaea',
    base_3 = '#888888',
    base_4 = '#111111',
    base_5 = '#000000',
    --
    red_1 = '#cc5d5d',
    green_1 = '#448844',
    yellow_1 = '#998855',
    blue_1 = '#4466aa',
    magenta_1 = '#bb88aa',
    cyan_1 = '#66aaaa',
    purple = '#8888cc',
    black = '#222222',
    white = '#ffffff',
    --
    red_2 = '#ee7878',
    green_2 = '#88bb88',
    yellow_2 = '#f0e0c0',
    blue_2 = '#aaccdd',
    magenta_2 = '#eeaadd',
    cyan_2 = '#aeeeee',
    --
    green_3 = '#ffeaff',
    cyan_3 = '#eaffff',
}

local colorscheme = {
    background = colors.base_0,
    foreground = colors.base_4,
    cursor_bg = colors.base_5,
    cursor_fg = colors.base_0,
    selection_bg = colors.yellow_2,
    selection_fg = colors.base_5,
    ansi = {
        colors.black,
        colors.red_1,
        colors.green_1,
        colors.yellow_1,
        colors.blue_1,
        colors.magenta_1,
        colors.cyan_1,
        colors.base_2,
    },
    brights = {
        colors.base_3,
        colors.red_2,
        colors.green_2,
        colors.yellow_2,
        colors.blue_2,
        colors.magenta_2,
        colors.cyan_2,
        colors.white,
    },
    tab_bar = {
        background = colors.base_1,
        active_tab = {
            bg_color = colors.base_1,
            fg_color = colors.base_5,
        },
        inactive_tab = {
            bg_color = colors.base_1,
            fg_color = colors.base_3,
        },
        inactive_tab_hover = {
            bg_color = colors.blue_2,
            fg_color = colors.base_4,
        },
        new_tab = {
            bg_color = colors.base_1,
            fg_color = colors.base_4,
        },
        new_tab_hover = {
            bg_color = colors.blue_2,
            fg_color = colors.base_4,
        },
    },
}

return colorscheme
