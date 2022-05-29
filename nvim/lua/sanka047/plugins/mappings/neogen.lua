--------------------------------------------------------------------------------
-- Neogen Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    map('n', '<leader>dg', 'generate documentation', function () require('neogen').generate() end)
end

return M
