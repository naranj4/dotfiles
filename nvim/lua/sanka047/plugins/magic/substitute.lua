--------------------------------------------------------------------------------
-- Substitute Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local ok, substitute = pcall(require, 'substitute')
    if not ok then
        log.error('substitute not available', 'Config')
        return false
    end

    substitute.setup({
        yank_substitued_text = false,
        range = {
            prefix = 'S',
        },
    })
end

--------------------------------------------------------------------------------
-- Substitute Keymap
--------------------------------------------------------------------------------
function M.keymap()
    -- Substitute Mappings
    map('n', '<leader>s', 'substitute', function () require('substitute').operator() end)
    map('n', '<leader>ss', 'substitute (line)', function () require('substitute').line() end)
    map('n', '<leader>S', 'substitute (eol)', function () require('substitute').eol() end)
    map('x', '<leader>s', 'substitute', function () require('substitute').visual() end)

    -- Exchange Mappings
    map('n', '<leader>x', 'exchange', function () require('substitute.exchange').operator() end)
    map('n', '<leader>xx', 'exchange (line)', function () require('substitute.exchange').line() end)
    map('n', '<leader>xc', 'exchange (cancel)', function () require('substitute.exchange').cancel() end)
    map('x', '<leader>x', 'exchange', function () require('substitute.exchange').visual() end)
end

return M
