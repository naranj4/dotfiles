--------------------------------------------------------------------------------
-- Hop Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map

map('', '<leader>j', 'Hop Down', "<CMD>lua require('hop').hint_lines({ direction = require('hop.hint').HintDirection.AFTER_CURSOR })<CR>")
map('', '<leader>k', 'Hop Up', "<CMD>lua require('hop').hint_lines({ direction = require('hop.hint').HintDirection.BEFORE_CURSOR })<CR>")
