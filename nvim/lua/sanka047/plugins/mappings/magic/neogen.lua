--------------------------------------------------------------------------------
-- Neogen Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

function M.keymap()
    map_group('n', '<leader>d', 'document')
    map('n', '<leader>dc', 'Document Class', function ()
        require('neogen').generate({ type = 'class' })
    end)
    map('n', '<leader>df', 'Document Function', function ()
        require('neogen').generate({ type = 'func' })
    end)
    map('n', '<leader>dt', 'Document Type', function ()
        require('neogen').generate({ type = 'type' })
    end)
    map('n', '<leader>dm', 'Document File/Module', function ()
        require('neogen').generate({ type = 'file' })
    end)
end

return M
