--------------------------------------------------------------------------------
-- Bqf Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, bqf = pcall(require, 'bqf')
    if not ok then
        log.error('bqf not available', 'Config')
        return false
    end

    bqf.setup({})
end

return M
