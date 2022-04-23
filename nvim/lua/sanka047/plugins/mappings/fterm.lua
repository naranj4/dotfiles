--------------------------------------------------------------------------------
-- FTerm Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map
local create_command = require('sanka047.core.utils').create_command

create_command(
    'FTermToggle',
    function ()
        require('FTerm').toggle()
    end,
    'Toggle FTerm'
)

map('n', '<A-i>', 'Toggle FTerm', "<CMD>FTermToggle<CR>")
map('t', '<A-i>', 'Toggle FTerm', "<C-\\><C-n><CMD>FTermToggle<CR>")
