--------------------------------------------------------------------------------
-- LSP Installer Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

function M.keymap()
    map_group('n', '<leader>g', 'lsp-get')

    map('n', '<leader>rn', 'Rename', vim.lsp.buf.rename)

    map('n', '<leader>gd', 'Get Def.', function () require("telescope.builtin").lsp_definitions() end)
    map('n', '<leader>gD', 'Get Decl.', vim.lsp.buf.declaration)
    map('n', '<leader>gy', 'Get Type Def.', function () require("telescope.builtin").lsp_type_definitions() end)
    map('n', '<leader>gi', 'Get Impl.', function () require("telescope.builtin").lsp_implementations() end)
    map('n', '<leader>gr', 'Get Ref.', function () require("telescope.builtin").lsp_references() end)

    map_group('n', '<leader>l', 'lsp')

    -- diagnostic information
    map('n', '<leader>ld', 'Find Diagnostics', function () require("telescope.builtin").diagnostics() end)
    map('n', '[d', 'Prev Diagnostic', vim.diagnostic.goto_prev)
    map('n', ']d', 'Next Diagnostic', vim.diagnostic.goto_next)
    map('n', '<leader>q', 'Diagnostic (Loc)List', vim.diagnostic.setloclist)

    -- workspace operations
    map_group('n', '<leader>w', 'workspace')
    map('n', '<leader>wa', 'Add Folder', vim.lsp.buf.add_workspace_folder)
    map('n', '<leader>wr', 'Remove Folder', vim.lsp.buf.remove_workspace_folder)
    map('n', '<leader>wl', 'List Folders', function () P(vim.lsp.buf.list_workspace_folders()) end)

    map('n', '<leader>lf', 'Format', vim.lsp.buf.formatting)

    map('n', '<leader>lca', 'Code Actions', function () require("telescope.builtin").lsp_code_actions() end)
    map('v', '<leader>lca', 'Code Actions', function () require("telescope.builtin").lsp_range_code_actions() end)
end

return M
