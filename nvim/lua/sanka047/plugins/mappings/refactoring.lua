--------------------------------------------------------------------------------
-- Refactoring Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

function M.keymap()
    map_group('nv', '<leader>r', 'refactor')
    map(
        'v',
        '<leader>rr',
        'Refactor (Telescope)',
        function () require('sanka047.utils.refactor').select_refactor() end
    )
    map(
        'n',
        '<leader>ri',
        'Inline Variable',
        function () require('sanka047.utils.refactor').inline_variable() end
    )
end

return M
