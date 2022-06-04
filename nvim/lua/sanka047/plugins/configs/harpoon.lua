--------------------------------------------------------------------------------
-- Harpoon Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, harpoon = pcall(require, 'harpoon')
    if not ok then
        log.error('harpoon not available', 'Config')
        return false
    end

    harpoon.setup({
        global_settings = {
            excluded_filetypes = { 'harpoon', 'TelescopePrompt', 'DressingInput' },
        },
    })
end

return M
