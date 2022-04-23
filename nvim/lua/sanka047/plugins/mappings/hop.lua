--------------------------------------------------------------------------------
-- Hop Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map
local create_command = require('sanka047.core.utils').create_command

create_command(
    'HopDown',
    function ()
        require('hop').hint_lines({ direction = require('hop.hint').HintDirection.AFTER_CURSOR })
    end,
    'Hop Down'
)
create_command(
    'HopUp',
    function ()
        require('hop').hint_lines({ direction = require('hop.hint').HintDirection.BEFORE_CURSOR })
    end,
    'Hop Up'
)

map('', '<leader>j', 'Hop Down', "<CMD>HopDown<CR>")
map('', '<leader>k', 'Hop Up', "<CMD>HopUp<CR>")
