--------------------------------------------------------------------------------
-- FTerm Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local create_command = require('sanka047.utils.map').create_command

local M = {}

function M.keymap()
    create_command(
        'FTermToggle',
        function ()
            require('FTerm').toggle()
        end,
        'Toggle FTerm'
    )

    map('n', '<A-i>', 'Toggle FTerm', "<CMD>FTermToggle<CR>")
    map('t', '<A-i>', 'Toggle FTerm', "<C-\\><C-n><CMD>FTermToggle<CR>")
end

return M
