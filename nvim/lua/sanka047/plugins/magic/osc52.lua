--------------------------------------------------------------------------------
-- OSC52 Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local has_osc52, osc52 = pcall(require, 'osc52')
    if not has_osc52 then
        log.error('osc52 not available', 'Config')
        return false
    end

    osc52.setup({ trim = true })
end

--------------------------------------------------------------------------------
-- OSC52 Keymap
--------------------------------------------------------------------------------
function M.keymap()
    map('n', '<C-y>', 'osc52 yank', require('osc52').copy_operator, { expr = true })
    map('n', '<C-y><C-y>', 'osc52 yank line', '<C-y>_', { remap = true })
    map('x', '<C-y>', 'osc52 yank', require('osc52').copy_visual)
end

return M
