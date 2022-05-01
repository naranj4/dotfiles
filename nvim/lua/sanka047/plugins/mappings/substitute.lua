--------------------------------------------------------------------------------
-- Telescope Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map

-- Substitute Mappings
map('n', 's', 'substitute', "<CMD>lua require('substitute').operator()<CR>")
map('n', 'ss', 'substitute (line)', "<CMD>lua require('substitute').line()<CR>")
map('n', 'S', 'substitute (eol)', "<CMD>lua require('substitute').eol()<CR>")
map('x', 's', 'substitute', "<CMD>lua require('substitute').visual()<CR>")

-- Exchange Mappings
map('n', 'sx', 'exchange', "<CMD>lua require('substitute.exchange').operator()<CR>")
map('n', 'sxx', 'exchange (line)', "<CMD>lua require('substitute.exchange').line()<CR>")
map('x', 'X', 'exchange', "<CMD>lua require('substitute.exchange').visual()<CR>")
map('n', 'sxc', 'exchange (cancel)', "<CMD>lua require('substitute.exchange').cancel()<CR>")
