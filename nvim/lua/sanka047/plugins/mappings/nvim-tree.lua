--------------------------------------------------------------------------------
-- Nvim-Tree Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

map_group('n', '<leader>n', 'nvim-tree')
map('n', '<leader>nn', 'Toggle', '<CMD>NvimTreeToggle<CR>')
map('n', '<leader>nr', 'Refresh', '<CMD>NvimTreeRefresh<CR>')
map('n', '<leader>nf', 'Find File', '<CMD>NvimTreeFindFileToggle<CR>')
