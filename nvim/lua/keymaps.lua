--------------------------------------------------------------------------------
-- Basic Neovim Key Maps (plugin specific mapping found in plugin files)
--------------------------------------------------------------------------------
local api = vim.api
local cmd = vim.cmd                 -- execute vim commands
local exec = vim.api.nvim_exec      -- execute vimscript

local common = require('common')
local map = common.map

local wk = require('which-key')

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------
-- document <leader> key
wk.register({ ['<leader>'] = { name = 'userspace'} }, { mode = '' })

wk.register({
    v = {
        name = 'config',
        e = { '<CMD>edit ~/.config/nvim/init.lua<CR>', 'Edit Neovim Config' },
    },
}, { mode = 'n', prefix = '<leader>' })

--------------------------------------------------------------------------------
-- Quality of Life
--------------------------------------------------------------------------------
wk.register({
    Q = { '', '' }, -- Q enters Ex mode, don't need

    ['<leader>Q'] = { '<CMD>bufdo bdelete<CR>', 'Delete All Buffers'},
    ['gf'] = { '<CMD>edit <cfile><CR>', 'Go To File' }, -- allow gf to open nonexistent files
    ['<ESC>'] = { ':nohlsearch<CR><ESC>', '<ESC>' }, -- remove highlighting

    [';'] = { ':', 'Command' },
    [':'] = { ';', 'f/F/t/T Repeat' },
}, { mode = 'n'})

wk.register({
    [';'] = { ':', 'Command' },
    [':'] = { ';', 'f/F/t/T Repeat' },
}, { mode = 'v' })

wk.register({
    H = { '^', 'Home' },
    L = { '$', 'End' },
}, { mode = '' })

wk.register({ ['kj'] = { '<ESC>', '<ESC>' }}, { mode = 'i' })

--------------------------------------------------------------------------------
-- Yank and Visual Selection
--------------------------------------------------------------------------------
wk.register({
    -- reselect visual selection after indenting
    ['<'] = { '<gv', 'Dedent' },
    ['>'] = { '>gv', 'Indent' },

    -- paste replace visual selection without copying it
    ['<leader>p'] = { '"_dp', 'Paste (w/o Copy)' },
}, { mode = 'v' })

wk.register({ Y = { 'y$', 'Yank to EOL' }}, { mode = 'n'}) -- make Y behave like other capitals

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

wk.register({
    ['<C-H>'] = { '<CMD>lua require("common").win_move("h")<CR>', 'W-Move/Create Left' },
    ['<C-J>'] = { '<CMD>lua require("common").win_move("j")<CR>', 'W-Move/Create Down' },
    ['<C-K>'] = { '<CMD>lua require("common").win_move("k")<CR>', 'W-Move/Create Up' },
    ['<C-L>'] = { '<CMD>lua require("common").win_move("l")<CR>', 'W-Move/Create Right' },
}, { mode = 'n' })

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
wk.register({
    ['<c-q>'] = { '<CMD>lua require("common").toggle_qf_list(true)<CR>', 'Toggle QF List' },
    ['<m-j>'] = { '<CMD>cnext<CR>', 'QF Next' },
    ['<m-k>'] = { '<CMD>cprev<CR>', 'QF Prev' },
}, { mode = 'n' })
