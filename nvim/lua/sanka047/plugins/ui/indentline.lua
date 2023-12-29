--------------------------------------------------------------------------------
-- Indent-Blankline Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, indent_blankline = pcall(require, 'ibl')
    if not ok then
        log.error('indent-blankline not available', 'Config')
        return false
    end

    indent_blankline.setup({
        exclude = {
            buftypes = { 'terminal', 'prompt', 'nofile', 'help' },
            filetypes = { 'dashboard', 'alpha', 'packer', 'lspinfo' },
        },
    })
end

return M
