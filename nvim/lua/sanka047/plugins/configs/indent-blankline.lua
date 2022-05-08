--------------------------------------------------------------------------------
-- Indent-Blankline Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, indent_blankline = pcall(require, 'indent_blankline')
    if not ok then
        log.error('indent-blankline not available', 'Config')
        return false
    end

    indent_blankline.setup({
        space_char_blankline = ' ',
        show_current_context = true,
        show_current_context_start = false,
        show_end_of_line = false,
        buftype_exclude = { 'terminal', 'prompt', 'nofile' },
        filetype_exclude = { 'dashboard', 'packer', 'lspinfo' },
    })
end

return M
