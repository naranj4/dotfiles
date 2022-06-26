--------------------------------------------------------------------------------
-- Refactoring Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

function M.setup()
    local ok, refactoring = pcall(require, 'refactoring')
    if not ok then
        log.error('refactoring not available', 'Config')
        return false
    end

    refactoring.setup({})

    local has_telescope, telescope = pcall(require, 'telescope')
    if not has_telescope then
        log.error('telescope not available to setup refactoring', 'Config')
        return false
    end

    telescope.load_extension('refactoring')
end

--------------------------------------------------------------------------------
-- Refactoring Keymap
--------------------------------------------------------------------------------
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
