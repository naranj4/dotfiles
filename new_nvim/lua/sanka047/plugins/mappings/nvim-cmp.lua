--------------------------------------------------------------------------------
-- Lualine Config
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map

map('ic', '<C-p>', 'Select Prev')
map('ic', '<C-n>', 'Select Next')
map('ic', '<S-Tab>', 'Select Prev')
map('ic', '<Tab>', 'Select Next')

map('ic', '<C-d>', 'Scroll Down (Docs)')
map('ic', '<C-f>', 'Scroll Up (Docs)')

map('ic', '<C-e>', 'Close')
map('ic', '<CR>', 'Confirm')
map('ic', '<C-Space>', 'Trigger Completion')

map('ic', '<C-j>', 'Snippet Hop Forward')
map('ic', '<C-k>', 'Snippet Hop Backward')
