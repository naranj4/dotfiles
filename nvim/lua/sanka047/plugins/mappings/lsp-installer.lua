--------------------------------------------------------------------------------
-- LSP Installer Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map
local map_group = require('sanka047.core.utils').map_group

map_group('n', '<leader>g', 'lsp-get')

map('n', '<leader>rn', 'Rename', '<CMD>lua vim.lsp.buf.rename()<CR>')

map('n', '<leader>gd', 'Get Def.', '<CMD>lua require("telescope.builtin").lsp_definitions()<CR>')
map('n', '<leader>gD', 'Get Decl.', '<CMD>lua vim.lsp.buf.declaration()<CR>')
map('n', '<leader>gy', 'Get Type Def.', '<CMD>lua require("telescope.builtin").lsp_type_definitions()<CR>')
map('n', '<leader>gi', 'Get Impl.', '<CMD>lua require("telescope.builtin").lsp_implementations()<CR>')
map('n', '<leader>gr', 'Get Ref.', '<CMD>lua require("telescope.builtin").lsp_references()<CR>')

map_group('n', '<leader>l', 'lsp')

-- diagnostic information
map('n', '<leader>ld', 'Find Diagnostics', '<CMD>lua require("telescope.builtin").diagnostics()<CR>')
map('n', '[d', 'Prev Diagnostic', '<CMD>lua vim.diagnostic.goto_prev()<CR>')
map('n', ']d', 'Next Diagnostic', '<CMD>lua vim.diagnostic.goto_next()<CR>')
map('n', '<leader>q', 'Diagnostic (Loc)List', '<CMD>lua vim.diagnostic.setloclist()<CR>')

-- workspace operations
map_group('n', '<leader>w', 'workspace')
map('n', '<leader>wa', 'Add Folder', '<CMD>lua vim.lsp.buf.add_workspace_folder()<CR>')
map('n', '<leader>wr', 'Remove Folder', '<CMD>lua vim.lsp.buf.remove_workspace_folder()<CR>')
map('n', '<leader>wl', 'List Folders', '<CMD>lua P(vim.lsp.buf.list_workspace_folders())<CR>')

map('n', '<leader>lf', 'Format', '<CMD>lua vim.lsp.buf.formatting()<CR>')

map('n', '<leader>lca', 'Code Actions', '<CMD>lua require("telescope.builtin").lsp_code_actions()<CR>')
map('v', '<leader>lca', 'Code Actions', '<CMD>lua require("telescope.builtin").lsp_range_code_actions()<CR>')
