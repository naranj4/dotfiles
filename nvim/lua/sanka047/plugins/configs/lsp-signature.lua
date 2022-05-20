--------------------------------------------------------------------------------
-- LSP Installer Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, lsp_signature = pcall(require, 'lsp_signature')
    if not ok then
        log.error('lsp_signature not available', 'Config')
        return false
    end

    lsp_signature.setup({
        bind = true,
        handler_opts = { border = 'rounded' },
        doc_lines = 10,
        floating_window = true,
        floating_window_above_cur_line = true,
        floating_window_off_x = 1,
        floating_window_off_y = 1,
        fix_pos = false,
        hint_enable = true,
        hint_prefix = ' ',
    })
end

return M
