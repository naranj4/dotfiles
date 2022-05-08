--------------------------------------------------------------------------------
-- FTerm Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, fterm = pcall(require, 'FTerm')
    if not ok then
        log.error('FTerm not available', 'Config')
        return false
    end

    fterm.setup({
        border = 'rounded',
        blend = 0,
        dimensions = {
            height = 0.9,
            width = 0.9,
        },
    })
end

return M
