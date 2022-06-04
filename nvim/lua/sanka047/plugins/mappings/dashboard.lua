--------------------------------------------------------------------------------
-- Dashboard Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

function M.keymap()
    map_group('n', '<leader><leader><leader>s', 'session')
    map('n', '<leader><leader><leader>ss', 'Session Save', ':<C-u>SessionSave<CR>')
    map('n', '<leader><leader><leader>sl', 'Session Load', ':<C-u>SessionLoad<CR>')

    map('n', '<leader><leader>nf', 'Create New File', '<CMD>DashboardNewFile<CR>')
end

return M
