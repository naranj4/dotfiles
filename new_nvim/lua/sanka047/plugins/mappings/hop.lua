--------------------------------------------------------------------------------
-- Hop Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map

vim.cmd([[
    command! MyHopDown lua require('hop').hint_lines({ direction = require('hop.hint').HintDirection.AFTER_CURSOR })
]])
vim.cmd([[
    command! MyHopUp lua require('hop').hint_lines({ direction = require('hop.hint').HintDirection.BEFORE_CURSOR })
]])

-- Jump to first non-whitespace character (without skipping blank lines)
map('', '<leader>j', 'Hop Down', ':MyHopDown<CR>^')
map('', '<leader>k', 'Hop Up', ':MyHopUp<CR>^')
