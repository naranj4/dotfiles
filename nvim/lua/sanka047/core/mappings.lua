--------------------------------------------------------------------------------
-- Neovim Keymaps
--------------------------------------------------------------------------------
local utils = require('sanka047.utils')
local window = require('sanka047.utils.window')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------
-- document <leader> key
map_group('nv', '<leader>', 'userspace')
map_group('nv', '<leader><leader>', 'second')

map_group('n', '<leader><leader>v', 'config')
map('n', '<leader><leader>ve', 'Edit Neovim Config', '<CMD>edit ~/.config/nvim/init.lua<CR>')
map('n', '<leader><leader>vr', 'Reload Neovim Config', utils.reload_config)

--------------------------------------------------------------------------------
-- Quality of Life
--------------------------------------------------------------------------------
map('nv', 'Q', 'gq', 'Format')

map('n', '<leader>Q', 'Delete All Buffers', '<CMD>bufdo bdelete<CR>')
map('n', 'gf', 'Go To File', '<CMD>edit <cfile><CR>') -- allow gf to open nonexistent files
map('n', '<leader><leader>nf', 'Open Empty File', '<CMD>enew<CR>')

map('n', '<ESC>', '<ESC>', ':nohlsearch<CR><ESC>') -- remove highlighting

map('nv', ';', '<CMD>', ':')
map('nv', ':', 'f/F/t/T Repeat', ';')

map('', 'H', 'Home', '^')
map('', 'L', 'End', '$')

-- quick escape in insert/select mode
map('is', 'kj', 'Switch to Normal', '<ESC>')

-- set undo breakpoints
map('i', ',', 'Undo Breakpoint', ',<C-g>u')
map('i', '.', 'Undo Breakpoint', '.<C-g>u')
map('i', '!', 'Undo Breakpoint', '!<C-g>u')
map('i', '?', 'Undo Breakpoint', '?<C-g>u')

-- move selected region
map('v', 'J', 'Move Region Down', ":m '>+1<CR>gv=gv")
map('v', 'K', 'Move Region Up', ":m '<-2<CR>gv=gv")

--------------------------------------------------------------------------------
-- Toggle Group
--------------------------------------------------------------------------------
map_group('n', '<leader><leader>t', 'toggle')

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

map('n', '<c-h>', 'W-Move/Create Left', function () window.win_move('h') end)
map('n', '<c-j>', 'W-Move/Create Down', function () window.win_move('j') end)
map('n', '<c-k>', 'W-Move/Create Up', function () window.win_move('k') end)
map('n', '<c-l>', 'W-Move/Create Right', function () window.win_move('l') end)

--------------------------------------------------------------------------------
-- Quickfix Shortcuts
--------------------------------------------------------------------------------
-- global list
map('n', '<c-q>', 'Toggle QF List', function () window.toggle_qf_list(true) end)
map('n', ']q', 'QF Next', '<CMD>cnext<CR>')
map('n', '[q', 'QF Prev', '<CMD>cprev<CR>')

local create_augroup = require('sanka047.utils.map').create_augroup
local create_autocmd = require('sanka047.utils.map').create_autocmd

create_augroup('fixlist')
create_autocmd(
    'BufWinEnter',
    'Sets Quickfix control variable',
    {
        group = 'fixlist',
        pattern = 'quickfix',
        callback = window.set_qf_control_var,
    }
)

--------------------------------------------------------------------------------
-- Terminal Mode Mappings
--------------------------------------------------------------------------------
map('t', 'KJ', 'Switch to Normal', '<C-\\><C-n>')

--------------------------------------------------------------------------------
-- Plugin Specific (mappings that have to be loaded prior to init)
--------------------------------------------------------------------------------
local plugins = {
    -- 'git.neogit',
    'lsp.luasnip',
    'magic.comment',
    'magic.neogen',
    'magic.refactoring',
    'magic.substitute',
    'nav.hop',
    'nav.telescope',
    'ui.nvim-tree',
}

for _, plugin in pairs(plugins) do
    LOAD_MAPPING(plugin)
end
