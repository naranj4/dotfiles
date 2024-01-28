--------------------------------------------------------------------------------
-- Textobjects Config
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.config()
    return {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = '@class.inner',
            },
            selection_modes = {
                ['@function.outer'] = 'V',
                ['@class.outer'] = 'V'
            }
        },
        move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
                [']m'] = '@function.outer',
                [']]'] = '@class.outer',
            },
            goto_next_end = {
                [']M'] = '@function.outer',
                [']['] = '@class.outer',
            },
            goto_prev_start = {
                ['[m'] = '@function.outer',
                ['[['] = '@class.outer',
            },
            goto_prev_end = {
                ['[M'] = '@function.outer',
                ['[]'] = '@class.outer',
            },
        },
    }
end

function M.keymap()
    -- Text Objects (Select)
    map('vo', 'af', 'outer function')
    map('vo', 'if', 'inner function')
    map('vo', 'ac', 'outer class')
    map('vo', 'ic', 'inner class')

    -- Text Objects (Movement)
    map('', ']m', 'Next Method Start')
    map('', '[m', 'Prev Method Start')
    map('', ']]', 'Next Class Start')
    map('', '[[', 'Prev Class Start')
    map('', ']M', 'Next Method End')
    map('', '[M', 'Prev Method End')
    map('', '][', 'Next Class End')
    map('', '[]', 'Prev Class End')
end

return M
