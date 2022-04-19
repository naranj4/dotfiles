--------------------------------------------------------------------------------
-- Neovim Keymaps
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map
local map_group = require('sanka047.core.utils').map_group
local unmap = require('sanka047.core.utils').unmap

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------
-- document <leader> key
map_group('nv', '<leader>', 'userspace')
map_group('nv', '<leader><leader>', 'second')

map_group('n', '<leader><leader>v', 'config')
map('n', '<leader><leader>ve', 'Edit Neovim Config', '<CMD>edit ~/.config/nvim/init.lua<CR>')
map('n', '<leader><leader>vr', 'Reload Neovim Config', '<CMD>lua require("sanka047.core.utils").reload_config()<CR>')

--------------------------------------------------------------------------------
-- Quality of Life
--------------------------------------------------------------------------------
unmap('n', 'Q') -- Q enters Ex mode, don't need

map('n', '<leader>Q', 'Delete All Buffers', '<CMD>bufdo bdelete<CR>')
map('n', 'gf', 'Go To File', '<CMD>edit <cfile><CR>') -- allow gf to open nonexistent files
map('n', '<ESC>', '<ESC>', ':nohlsearch<CR><ESC>') -- remove highlighting

map('nv', ';', '<CMD>', ':')
map('nv', ':', 'f/F/t/T Repeat', ';')

map('', 'H', 'Home', '^')
map('', 'L', 'End', '$')

-- quick escape in insert mode
map('i', 'kj', '<ESC>', '<ESC>')

-- set undo breakpoints
map('i', ',', 'Undo Breakpoint', ',<C-g>u')
map('i', '.', 'Undo Breakpoint', '.<C-g>u')
map('i', '!', 'Undo Breakpoint', '!<C-g>u')
map('i', '?', 'Undo Breakpoint', '?<C-g>u')

-- move selected region
map('v', 'J', 'Move Region Down', ":m '>+1<CR>gv=gv")
map('v', 'K', 'Move Region Up', ":m '<-2<CR>gv=gv")

--------------------------------------------------------------------------------
-- Yank and Visual Selection
--------------------------------------------------------------------------------
-- reselect visual selection after indenting
map('v', '<', 'Dedent', '<gv')
map('v', '>', 'Indent', '>gv')

-- paste replace visual selection without copying it
map('v', '<leader>p', 'Paste (w/o Copy)', '"_dp')
map('v', '<leader>P', 'Paste (w/o Copy)', '"_dP')

map('n', 'Y', 'Yank to EOL', 'y$') -- make Y behave like other capitals

--------------------------------------------------------------------------------
-- Quick Tab/Window Commands
--------------------------------------------------------------------------------
map('n', '<Right>', 'Next Tab', '<CMD>tabnext<CR>')
map('n', '<Left>', 'Prev Tab', '<CMD>tabprevious<CR>')

map('n', '<c-h>', 'W-Move/Create Left', '<CMD>lua require("sanka047.core.utils").win_move("h")<CR>')
map('n', '<c-j>', 'W-Move/Create Down', '<CMD>lua require("sanka047.core.utils").win_move("j")<CR>')
map('n', '<c-k>', 'W-Move/Create Up', '<CMD>lua require("sanka047.core.utils").win_move("k")<CR>')
map('n', '<c-l>', 'W-Move/Create Right', '<CMD>lua require("sanka047.core.utils").win_move("l")<CR>')

--------------------------------------------------------------------------------
-- Quickfix Shortcuts
--------------------------------------------------------------------------------
-- global list
map('n', '<c-q>', 'Toggle QF List', '<CMD>lua require("sanka047.core.utils").toggle_qf_list(true)<CR>')
map('n', '<m-j>', 'QF Next', '<CMD>cnext<CR>')
map('n', '<m-k>', 'QF Prev', '<CMD>cprev<CR>')

--------------------------------------------------------------------------------
-- Terminal Mode Mappings
--------------------------------------------------------------------------------
map('t', 'kj', '<ESC>', '<C-\\><C-n>')

--------------------------------------------------------------------------------
-- Plugin Specific (mappings that have to be loaded prior to init)
--------------------------------------------------------------------------------
local plugins = {
    'nvim-tree',
    'hop',
    'telescope',
}

for _, plugin in pairs(plugins) do
    LOAD_MAPPING(plugin)
end
