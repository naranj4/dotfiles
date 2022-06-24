--------------------------------------------------------------------------------
-- Hop Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    map(
        '',
        '<leader>j',
        'Hop Down',
        function ()
            require('hop').hint_lines({ direction = require('hop.hint').HintDirection.AFTER_CURSOR })
            vim.cmd('norm ^')
        end
    )
    map(
        '',
        '<leader>k',
        'Hop Up',
        function ()
            require('hop').hint_lines({ direction = require('hop.hint').HintDirection.BEFORE_CURSOR })
            vim.cmd('norm ^')
        end
    )
end

return M
