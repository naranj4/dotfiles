--------------------------------------------------------------------------------
-- Neogit Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    map('n', '<leader>ng', 'Open Neogit', function () require('neogit').open() end)
end

return M
