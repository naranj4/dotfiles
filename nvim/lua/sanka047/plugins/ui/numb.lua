--------------------------------------------------------------------------------
-- Numb Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, numb = pcall(require, 'numb')
    if not ok then
        log.error('numb not available', 'Config')
        return false
    end

    numb.setup()
end

return M
