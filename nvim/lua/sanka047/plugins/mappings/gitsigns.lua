--------------------------------------------------------------------------------
-- GitSigns Keymap Documentation
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

function M.keymap()
    map('', ']c', 'Next Hunk')
    map('', '[c', 'Prev Hunk')

    map_group('n', '<leader>h', 'git-signs-actions')
    map('nv', '<leader>hs', 'Stage Hunk')
    map('n', '<leader>hu', 'Undo Stage Hunk')
    map('nv', '<leader>hr', 'Reset Hunk')
    map('n', '<leader>hS', 'Stage Buffer')
    map('n', '<leader>hU', 'Undo Buffer Index')
    map('n', '<leader>hR', 'Reset Buffer')
    map('n', '<leader>hp', 'Preview Hunk')
    map('n', '<leader>hb', 'Blame Line')
end

return M
