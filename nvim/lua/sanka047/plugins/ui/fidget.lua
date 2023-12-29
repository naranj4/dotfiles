--------------------------------------------------------------------------------
-- Fidget Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, fidget = pcall(require, 'fidget')
    if not ok then
        log.error('fidget not available', 'Config')
        return false
    end

    fidget.setup({
        progress = {
            suppress_on_insert = true,
            display = {
                render_limit = 8,
                progress_icon = { pattern = 'dots_pulse', period = 2 },
            }
        },
        notification = {
            override_vim_notify = true,
        },
        logger = {
            level = vim.log.levels.WARN,
        },
    })
end

return M
