--------------------------------------------------------------------------------
-- Hop Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, hop = pcall(require, 'hop')
    if not ok then
        log.error('hop not available', 'Config')
        return false
    end

    hop.setup({
        create_hl_autocmd = false,
    })
end

return M
