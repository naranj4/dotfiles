--------------------------------------------------------------------------------
-- FTerm Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map

map('n', '<A-i>', 'Toggle FTerm', "<CMD>lua require('FTerm').toggle()<CR>")
map('t', '<A-i>', 'Toggle FTerm', "<C-\\><C-n><CMD>lua require('FTerm').toggle()<CR>")
