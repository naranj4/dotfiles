--------------------------------------------------------------------------------
-- Textsubjects Config
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.config()
    return {
        enable = true,
        prev_selection = '<M-,>',
        keymaps = {
            ['.'] = 'textsubjects-smart',
            ['<M-;>'] = 'textsubjects-container-outer',
            ['i<M-;>'] = 'textsubjects-container-inner',
        },
    }
end

function M.keymap()
    map('vo', '.', 'smart selection')
    map('vo', '<M-;>', 'container outer')
    map('vo', 'i<M-;>', 'container inner')
end

return M
