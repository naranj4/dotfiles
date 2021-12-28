--------------------------------------------------------------------------------
-- Basic Neovim Key Maps (plugin specific mapping found in plugin files)
--------------------------------------------------------------------------------
local api = vim.api
local cmd = vim.cmd                 -- execute vim commands
local exec = vim.api.nvim_exec      -- execute vimscript

local common = require('common')
local map = common.map

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------
map('n', '<leader>ve', '<CMD>edit ~/.config/nvim/init.lua<CR>')

--------------------------------------------------------------------------------
-- Quality of Life
--------------------------------------------------------------------------------
map('n', 'Q', '')  -- Q enters Ex mode, don't need

map('n', '<leader>Q', ':bufdo bdelete')
map('n', 'gf', ':edit <cfile><CR>')  -- allow gf to open nonexistent files

map('i', 'kj', '<ESC>')

map('n', ';', ':')
map('n', ':', ';')
map('v', ';', ':')
map('v', ':', ';')

map('n', '<ESC>', ':noh<CR><ESC>', {silent = true})  -- remove highlighting

map('', 'H', '^')
map('', 'L', '$')

--------------------------------------------------------------------------------
-- Yank and Visual Selection
--------------------------------------------------------------------------------
-- reselect visual selection after indenting
map('v', '<', '<gv')
map('v', '>', '>gv')

-- paste replace visual selection without copying it
map('v', '<leader>p', '"_dP')

map('n', 'Y', 'y$')  -- make Y behave like other capitals

--------------------------------------------------------------------------------
-- Quick Window Commands
--------------------------------------------------------------------------------
function common.win_move(direction)
    local curwin = api.nvim_win_get_number(0)
    cmd('wincmd ' .. direction)
    if curwin == api.nvim_win_get_number(0) then
        if direction == 'j' or direction == 'k' then
            cmd('wincmd s')
        else
            cmd('wincmd v')
        end
        cmd('wincmd ' .. direction)
    end
end

map('n', '<C-H>', '<CMD>lua require("common").win_move("h")<CR>', {silent = true})
map('n', '<C-J>', '<CMD>lua require("common").win_move("j")<CR>', {silent = true})
map('n', '<C-K>', '<CMD>lua require("common").win_move("k")<CR>', {silent = true})
map('n', '<C-L>', '<CMD>lua require("common").win_move("l")<CR>', {silent = true})

map('n', 'gp', ':wincmd P<CR>')
