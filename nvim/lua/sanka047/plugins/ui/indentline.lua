--------------------------------------------------------------------------------
-- Indent-Blankline Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, ibl = pcall(require, 'ibl')
    if not ok then
        log.error('indent-blankline not available', 'Config')
        return false
    end

    ibl.setup({
        indent = { tab_char = '▎' },
        exclude = {
            buftypes = { 'terminal', 'prompt', 'nofile', 'help' },
            filetypes = { 'dashboard', 'alpha', 'packer', 'lspinfo' },
        },
    })
end

return M
