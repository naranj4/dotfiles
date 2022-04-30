--------------------------------------------------------------------------------
-- Shade Config
--------------------------------------------------------------------------------
local ok, shade = pcall(require, 'shade')
if not ok then
    vim.notify('shade not available', 'error')
    return false
end

shade.setup({
    overlay_opacity = 55,
    opacity_step = 5,
    keys = {
        brightness_up = '<leader><leader>sbu',
        brightness_down = '<leader><leader>sbd',
        toggle = '<leader><leader>st',
    },
})
