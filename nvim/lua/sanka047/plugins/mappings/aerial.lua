--------------------------------------------------------------------------------
-- Aerial Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    map('n', '<leader>a', 'Aerial', '<CMD>AerialToggle<CR>')
end

return M
