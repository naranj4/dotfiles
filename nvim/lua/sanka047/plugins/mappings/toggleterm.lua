--------------------------------------------------------------------------------
-- Toggleterm Custom Terminals
--------------------------------------------------------------------------------
local ok, toggleterm = pcall(require, 'toggleterm')
if not ok then
    vim.notify('toggleterm not available', 'error')
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
local map = require('sanka047.utils.map').map

map('nt', '<M-i>', 'Toggle ZSH', function () zsh:toggle() end)
