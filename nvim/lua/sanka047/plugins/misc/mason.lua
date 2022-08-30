--------------------------------------------------------------------------------
-- Mason Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local window = require('sanka047.utils.window')

local M = {}

function M.setup()
    local has_mason, mason = pcall(require, 'mason')
    if not has_mason then
        log.error('mason not available', 'Config')
        return false
    end

    mason.setup({
        ui = {
            border = window.border(window.margin.FULL),
            icons = {
                server_installed = "✓",
                server_pending = "➜",
                server_uninstalled = "✗",
            },
        },
    })
end

return M
