--------------------------------------------------------------------------------
-- Shade Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, shade = pcall(require, 'shade')
    if not ok then
        log.error('shade not available', 'Config')
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
end

return M
