--------------------------------------------------------------------------------
-- Shade Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

function M.keymap()
    map_group('n', '<leader><leader><leader>sh', 'shade')
    map('n', '<leader><leader><leader>shu', 'Shade Brightness Up')
    map('n', '<leader><leader><leader>shd', 'Shade Brightness Down')
    map('n', '<leader><leader>tsh', 'Shade Toggle')
end

return M
