--------------------------------------------------------------------------------
-- Dashboard Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map
local map_group = require('sanka047.core.utils').map_group

map_group('n', '<leader><leader>s', 'session')
map('n', '<leader><leader>ss', 'Session Save', ':<C-u>SessionSave<CR>')
map('n', '<leader><leader>sl', 'Session Load', ':<C-u>SessionLoad<CR>')

map('n', '<leader><leader>nf', 'Create New File', '<CMD>DashboardNewFile<CR>')
