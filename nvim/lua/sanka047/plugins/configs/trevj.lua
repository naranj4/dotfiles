--------------------------------------------------------------------------------
-- TrevJ Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local has_trevj, trevj = pcall(require, 'trevj')
    if not has_trevj then
        log.error('trevj not available', 'Config')
        return false
    end

    trevj.setup({})
end

return M
