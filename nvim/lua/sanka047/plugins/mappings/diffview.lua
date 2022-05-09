--------------------------------------------------------------------------------
-- Diffview Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    map('n', '<leader><leader>gd', 'Open Diffview', ':DiffviewOpen<space>')
    map('n', '<leader><leader>gh', 'Open file history', ':DiffviewFileHistory<space>')
end

return M
