--------------------------------------------------------------------------------
-- TrevJ Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    map('n', '<leader>J', 'Reverse Join', function () require('trevj').format_at_cursor() end)
end

return M
