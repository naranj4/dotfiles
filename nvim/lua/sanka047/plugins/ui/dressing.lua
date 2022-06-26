--------------------------------------------------------------------------------
-- Dressing Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, dressing = pcall(require, 'dressing')
    if not ok then
        log.error('dressing not available', 'Config')
        return false
    end

    dressing.setup({
        input = {
            enabled = true,
            insert_only = true,
            winblend = 0,
            override = function (conf)
                conf['border'] = require('sanka047.utils.window').border(true)
            end
        },
        select = {
            enabled = true,
            backend = { "telescope", "builtin" },
            winblend = 0,
        },
    })
end

return M
