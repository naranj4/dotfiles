--------------------------------------------------------------------------------
-- Refactoring Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

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

return M
