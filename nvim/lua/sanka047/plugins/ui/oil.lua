--------------------------------------------------------------------------------
-- Oil Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map
local window = require('sanka047.utils.window')

local M = {}

function M.setup()
    local has_oil, oil = pcall(require, 'oil')
    if not has_oil then
        log.error('oil not available', 'Config')
        return false
    end

    oil.setup({
        columns = { 'icon', 'permissions', 'size', 'mtime' },
        float = {
            border = window.border(window.margin.FULL),
            win_options = { winblend = 0 },
        },
    })
end

function M.keymap()
    map('n', '_', 'Edit directory buffer (cwd)', function ()
        require('oil').open(vim.fn.getcwd())
    end)
    map('n', '-', 'Edit directory buffer (file)', function () require('oil').open() end)
end

return M
