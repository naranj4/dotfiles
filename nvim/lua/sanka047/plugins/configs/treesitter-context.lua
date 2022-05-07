--------------------------------------------------------------------------------
-- Treesitter-Context Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local ok, treesitter_context = pcall(require, 'treesitter-context')
if not ok then
    log.error('nvim-treesitter-context not available', 'Config')
    return false
end

treesitter_context.setup({
    enable = true,
    throttle = true,
    max_lines = 1,
    patterns = {
        default = {
            'class',
            'function',
            'method',
        },
    },
})
