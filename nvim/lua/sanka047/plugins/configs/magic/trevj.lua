--------------------------------------------------------------------------------
-- TrevJ Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local has_trevj, trevj = pcall(require, 'trevj')
    if not has_trevj then
        log.error('trevj not available', 'Config')
        return false
    end

    trevj.setup({})
end

--------------------------------------------------------------------------------
-- TrevJ Keymap
--------------------------------------------------------------------------------
function M.keymap()
    map('n', '<leader>J', 'Reverse Join', function () require('trevj').format_at_cursor() end)
end

return M
