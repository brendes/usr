local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local font_ui = 'Inter'

-- text
config.adjust_window_size_when_changing_font_size = false
config.font = wezterm.font('Input Mono')
config.line_height = 1.15
config.font_size = 13
config.freetype_load_flags = 'NO_HINTING|NO_AUTOHINT'
-- config.font_rasterizer = 'FreeType'
-- config.freetype_load_target = 'HorizontalLcd'

-- windows
config.window_frame = {
  font = wezterm.font { family = font_ui },
}
config.window_padding = {
  left = 30,
  right = 30,
  top = 30,
  bottom = 30,
}
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
-- default: 'TITLE | RESIZE'
config.window_decorations = 'RESIZE'

-- theme
local scheme_light = 'colors.calm'
local scheme_dark = 'colors.scotopic'

local function load_scheme(modname)
    package.loaded[modname] = nil
    return require(modname)
end

local function pick(appearance)
    if appearance:find 'Dark' then
        return load_scheme(scheme_dark)
    else
        return load_scheme(scheme_light)
    end
end


config.colors = pick(wezterm.gui.get_appearance())
config.color_scheme = nil

wezterm.on('window-config-reloaded', function(win, _)
    win:set_config_overrides{
        colors       = pick(win:get_appearance()),
        color_scheme = nil,
    }
end)

return config
