local colors = {
  base_0 = '#000000',
  base_1 = '#553333',
  base_2 = '#664444',
  base_3 = '#aa5555',
  base_4 = '#ee8888',
  base_5 = '#ff8888',
}

local colorscheme = {
  background = colors.base_0,
  foreground = colors.base_4,
  cursor_bg = colors.base_5,
  cursor_fg = colors.base_0,
  selection_bg = colors.base_2,
  selection_fg = colors.base_5,
  ansi = {
    '#110000',
    colors.base_4,
    colors.base_4,
    colors.base_4,
    colors.base_4,
    colors.base_4,
    colors.base_4,
    colors.base_1,
  },
  brights = {
    colors.base_3,
    colors.base_4,
    colors.base_4,
    colors.base_4,
    colors.base_4,
    colors.base_4,
    colors.base_4,
    colors.base_5,
  }
}

return colorscheme

-- [colors.tab_bar]
-- background = '#000000'
-- # inactive_tab_edge = '#ebe5df'
-- # inactive_tab_edge_hover = '#ebe0df'
--
-- [colors.tab_bar.active_tab]
-- bg_color = '#000000'
-- fg_color = '#ee8888'
--
-- [colors.tab_bar.inactive_tab]
-- bg_color = '#000000'
-- fg_color = '#aa5555'
--
-- # [colors.tab_bar.inactive_tab_hover]
-- # bg_color = '#553333'
--
-- [colors.tab_bar.new_tab]
-- bg_color = '#000000'
-- fg_color = '#ee8888'
--
-- [colors.tab_bar.new_tab_hover]
-- bg_color = '#553333'
-- fg_color = '#ee8888'
