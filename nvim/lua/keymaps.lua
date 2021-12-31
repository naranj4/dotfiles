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

map('n', '<ESC>', ':noh<CR><ESC>')  -- remove highlighting

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

map('n', '<C-H>', '<CMD>lua require("common").win_move("h")<CR>')
map('n', '<C-J>', '<CMD>lua require("common").win_move("j")<CR>')
map('n', '<C-K>', '<CMD>lua require("common").win_move("k")<CR>')
map('n', '<C-L>', '<CMD>lua require("common").win_move("l")<CR>')

--------------------------------------------------------------------------------
-- Quickfix Shortcuts
--------------------------------------------------------------------------------
common.is_qf_list_open = false
common.is_loc_list_open = false
function common.toggle_qf_list(is_qf)
    if is_qf then
        if common.is_qf_list_open then
            cmd('cclose')
        else
            cmd('copen')
        end
    else
        if common.is_loc_list_open then
            cmd('lclose')
        else
            cmd('lopen')
        end
    end
end

function common.set_qf_control_var()
    exec([[
        if getwininfo(win_getid())[0]['loclist'] == 1
            lua require('common').is_loc_list_open = true
        else
            lua require('common').is_qf_list_open = true
        end
    ]], false)
end

function common.unset_qf_control_var()
    exec([[
        if getwininfo(win_getid())[0]['loclist'] == 1
            lua require('common').is_loc_list_open = false
        else
            lua require('common').is_qf_list_open = false
        end
    ]], false)
end

exec([[
    augroup fixlist
        autocmd!
        autocmd BufWinEnter quickfix lua require('common').set_qf_control_var()
        autocmd BufWinLeave * lua require('common').unset_qf_control_var()
    augroup END
]], false)

-- global list
map('n', '<c-q>', '<CMD>lua require("common").toggle_qf_list(true)<CR>')
map('n', '<m-j>', '<CMD>cnext<CR>')
map('n', '<m-k>', '<CMD>cprev<CR>')
