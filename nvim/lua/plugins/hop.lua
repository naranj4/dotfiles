--------------------------------------------------------------------------------
-- Hop Config
--------------------------------------------------------------------------------
require('hop').setup({})

--------------------------------------------------------------------------------
-- Hop Keymap
--------------------------------------------------------------------------------
local common = require('common')
local map = common.map

map('', '<leader>j', '<CMD>lua require("hop").hint_lines_skip_whitespace()<CR>', {silent = true})
map('', '<leader>w', '<CMD>lua require("hop").hint_words()<CR>', {silent = true})
