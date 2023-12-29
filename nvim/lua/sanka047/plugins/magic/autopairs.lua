--------------------------------------------------------------------------------
-- Nvim-Autopairs Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, npairs = pcall(require, 'nvim-autopairs')
    if not ok then
        log.error('nvim-autopairs not available', 'Config')
        return false
    end

    npairs.setup({ check_ts = true })
end

return M
