--------------------------------------------------------------------------------
-- Notify Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, notify = pcall(require, 'notify')
    if not ok then
        log.error('notify not available', 'Config')
        return false
    end

    notify.setup({
        stages = "fade",
        render = "default",
    })

    vim.notify = notify
end

return M
