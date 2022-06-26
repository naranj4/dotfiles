--------------------------------------------------------------------------------
-- Shade Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

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
            brightness_up = '<leader><leader><leader>shu',
            brightness_down = '<leader><leader><leader>shd',
            toggle = '<leader><leader>tsh',
        },
    })
end

--------------------------------------------------------------------------------
-- Shade Keymap
--------------------------------------------------------------------------------
function M.keymap()
    map_group('n', '<leader><leader><leader>sh', 'shade')
    map('n', '<leader><leader><leader>shu', 'Shade Brightness Up')
    map('n', '<leader><leader><leader>shd', 'Shade Brightness Down')
    map('n', '<leader><leader>tsh', 'Shade Toggle')
end

return M
