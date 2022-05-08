--------------------------------------------------------------------------------
-- Comment Keymap Documentation
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    map('nx', '<leader>c', 'Line Comment')
    map('n', '<leader>cc', 'Toggle Line Comment')
    map('n', '<leader>co', 'Add Comment Above')
    map('n', '<leader>cO', 'Add Comment Below')
    map('n', '<leader>cA', 'Add Comment (EOL)')

    map('nx', '<leader>b', 'Block Comment')
    map('n', '<leader>bc', 'Toggle Block Comment')
end

return M
