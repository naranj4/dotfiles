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
        stages = "slide",
        render = "default",
        on_open = function (win)
            local buf = vim.api.nvim_win_get_buf(win)
            vim.api.nvim_buf_set_option(buf, 'filetype', 'markdown')
        end
    })

    vim.notify = notify
end

return M
