--------------------------------------------------------------------------------
-- Leap Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local has_leap, leap = pcall(require, 'leap')
    if not has_leap then
        log.error('leap not available', 'Config')
        return false
    end

    leap.setup({
        safe_labels = {}, -- autojump is disorienting with scrolloff 999
        labels = {
            'j', 'f', 'k', 'd', 'l', ';', 'a', 'h', 'g',
            'u', 'r', 'i', 'e', 'o', 'p', 'q', 'y', 't',
            'm', 'v', ',', 'c', '.', '/', 'z', 'n', 'b',
            's', 'w', 'x', -- left ring finger lowest priority
        },
    })
end

function M.keymap()
    map('', 's', 'leap forward', '<Plug>(leap-forward)')
    map('', 'S', 'leap backward', '<Plug>(leap-backward)')
end

return M
