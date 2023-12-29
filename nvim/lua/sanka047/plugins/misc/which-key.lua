--------------------------------------------------------------------------------
-- Which Key Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local window = require('sanka047.utils.window')

local M = {}

function M.setup()
    local ok, which_key = pcall(require, 'which-key')
    if not ok then
        log.error('which-key not available', 'Config')
        return false
    end

    which_key.setup({
        plugins = {
            spelling = { enabled = false },
        },
        operators = {
            ['<leader>c'] = 'Comment',
            ['<leader>b'] = 'Comment Block',
        },
        window = {
            border = window.border(window.margin.FULL), -- none, single, double, shadow
            margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
            padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
        },
        triggers = {"<leader>"},
    })
end

return M
