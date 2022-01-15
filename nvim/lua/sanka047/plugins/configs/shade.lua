--------------------------------------------------------------------------------
-- Shade Config
--------------------------------------------------------------------------------
local ok, shade = pcall(require, 'shade')
if not ok then
    print('shade not available')
    return false
end

shade.setup({
    overlay_opacity = 40,
    opacity_step = 5,
    keys = {
        brightness_up = '<leader><leader>sbu',
        brightness_down = '<leader><leader>sbd',
        toggle = '<leader><leader>st',
    },
})
