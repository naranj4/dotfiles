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
        handler_opts = { border = require('sanka047.utils.window').border(true) },
        doc_lines = 10,
        -- FIX: turn this back on when #182 is fixed
        -- Issue: https://github.com/ray-x/lsp_signature.nvim/issues/182
        floating_window = false,
        fix_pos = false,
        hint_enable = true,
        hint_prefix = ' ',
        toggle_key = '<C-s>',
    })
end

return M
