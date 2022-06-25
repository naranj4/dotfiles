--------------------------------------------------------------------------------
-- Nvim-Tree Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

function M.setup()
    local ok, nvim_tree = pcall(require, 'nvim-tree')
    if not ok then
        log.error('nvim-tree not available', 'Config')
        return false
    end

    nvim_tree.setup({
        hijack_netrw = false,
        hijack_cursor = true,
        update_cwd = true,
        diagnostics = { enable = true },
        view = {
            width = 50,
            mappings = {
                list = { { key = '?', action = 'toggle_help' } },
            },
        },
        renderer = {
            indent_markers = { enable = true },
            highlight_git = true,
            highlight_opened_files = 'name',
            group_empty = true,
            add_trailing = true,
        },
        hijack_directories = { enable = false },
        actions = {
            open_file = { quit_on_open = true },
        },
    })
end

--------------------------------------------------------------------------------
-- Nvim-Tree Keymap
--------------------------------------------------------------------------------
function M.keymap()
    map_group('n', '<leader>n', 'nvim-tree')
    map('n', '<leader>nn', 'Toggle', '<CMD>NvimTreeToggle<CR>')
    map('n', '<leader>nr', 'Refresh', '<CMD>NvimTreeRefresh<CR>')
    map('n', '<leader>nf', 'Find File', '<CMD>NvimTreeFindFileToggle<CR>')
end

return M
