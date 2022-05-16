--------------------------------------------------------------------------------
-- Nvim-Gps Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, gps = pcall(require, 'nvim-gps')
    if not ok then
        log.error('nvim-gps not available', 'Config')
        return false
    end

    gps.setup({
        separator = '  ',
        depth = 3,
        depth_limit_indicator = '',
    })
end

return M
