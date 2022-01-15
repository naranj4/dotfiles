--------------------------------------------------------------------------------
-- Shade Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map
local map_group = require('sanka047.core.utils').map_group

map_group('n', '<leader><leader>s', 'shade')
map('n', '<leader><leader>sbu', 'Shade Brightness Up')
map('n', '<leader><leader>sbd', 'Shade Brightness Down')
map('n', '<leader><leader>st', 'Shade Toggle')
