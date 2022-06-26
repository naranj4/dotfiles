--------------------------------------------------------------------------------
-- Hop Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local ok, hop = pcall(require, 'hop')
    if not ok then
        log.error('hop not available', 'Config')
        return false
    end

    hop.setup({
        create_hl_autocmd = false,
    })
end

--------------------------------------------------------------------------------
-- Hop Keymap
--------------------------------------------------------------------------------
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
