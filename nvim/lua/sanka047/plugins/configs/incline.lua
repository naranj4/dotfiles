--------------------------------------------------------------------------------
-- Incline Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, incline = pcall(require, 'incline')
    if not ok then
        log.error('incline not available', 'Config')
        return false
    end

    incline.setup({
        window = {
            placement = {
                vertical = 'top',
                horizontal = 'right',
            },
            margin = {
                vertical = 0,
                horizontal = 1,
            },
            winhighlight = {
                active = { Normal = 'InclineNormal' },
                inactive = { Normal = 'InclineNormalNC' },
            },
            padding = 2,
        },
    })
end

return M
