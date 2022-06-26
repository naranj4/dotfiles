--------------------------------------------------------------------------------
-- Dirbuf Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local has_dirbuf, dirbuf = pcall(require, 'dirbuf')
    if not has_dirbuf then
        log.error('dirbuf not available', 'Config')
        return false
    end

    dirbuf.setup({
        hash_padding = 4,
        sort_order = 'directories_first',
        write_cmd = 'DirbufSync -confirm',
    })
end

function M.keymap()
    map('n', '<leader>nee', 'Edit directory buffer (cwd)', '<CMD>vertical topleft split | Dirbuf<CR>')
    map('n', '<leader>nef', 'Edit directory buffer (file)', '<CMD>vertical topleft split | Dirbuf %<CR>')
end

return M
