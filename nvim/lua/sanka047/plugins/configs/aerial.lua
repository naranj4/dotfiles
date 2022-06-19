--------------------------------------------------------------------------------
-- Aerial Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local has_aerial, aerial = pcall(require, 'aerial')
    if not has_aerial then
        log.error('aerial not available', 'Config')
        return false
    end

    aerial.setup({
        backends = { 'lsp', 'treesitter', 'markdown' },
        default_direction = 'right',
        post_jump_cmd = false,
        close_on_select = true,
        show_guides = true,
        highlight_mode = 'last',
    })
end

return M
