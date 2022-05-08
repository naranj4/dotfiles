--------------------------------------------------------------------------------
-- Toggleterm Custom Terminals
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    local ok, toggleterm = pcall(require, 'toggleterm')
    if not ok then
        log.error('toggleterm not available', 'Keymap')
        return false
    end

    local Terminal = require('toggleterm.terminal').Terminal

    -- simple zsh terminal
    local zsh = Terminal:new({
        direction = 'float',
        hidden = true,
    })

    --------------------------------------------------------------------------------
    -- Toggleterm Keymap
    --------------------------------------------------------------------------------
    map('nt', '<M-i>', 'Toggle ZSH', function () zsh:toggle() end)
end

return M
