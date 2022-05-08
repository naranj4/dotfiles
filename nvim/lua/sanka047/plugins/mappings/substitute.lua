--------------------------------------------------------------------------------
-- Substitute Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    -- Substitute Mappings
    map('n', 's', 'substitute', function () require('substitute').operator() end)
    map('n', 'ss', 'substitute (line)', function () require('substitute').line() end)
    map('n', 'S', 'substitute (eol)', function () require('substitute').eol() end)
    map('x', 's', 'substitute', function () require('substitute').visual() end)

    -- Exchange Mappings
    map('n', 'sx', 'exchange', function () require('substitute.exchange').operator() end)
    map('n', 'sxx', 'exchange (line)', function () require('substitute.exchange').line() end)
    map('x', 'X', 'exchange', function () require('substitute.exchange').visual() end)
    map('n', 'sxc', 'exchange (cancel)', function () require('substitute.exchange').cancel() end)
end

return M
