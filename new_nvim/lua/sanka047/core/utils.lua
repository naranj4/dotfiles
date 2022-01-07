--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------
local wk = require('which-key')

local M = {}

--------------------------------------------------------------------------------
-- Mapping Functions
--------------------------------------------------------------------------------
-- map keybind for all specified modes
function M.map(modes, keys, docstring, mapping, override_opts)
    if modes == '' then
        modes = 'nvo'
    end

    override_opts = override_opts or {}
    local opts = { noremap = true, silent = true }
    for i, v in pairs(override_opts) do
        opts[i] = v
    end

    for m in modes:gmatch('.') do
        if mapping ~= nil then
            vim.api.nvim_set_keymap(m, keys, mapping, opts)
        end

        -- Document the keymap using which keys immediately after mapping.
        -- wk has seemingly been pretty buggy with mapping for me, so checking to
        -- see if it will still work for documenting keymaps.
        wk.register({ [keys] = docstring }, { mode = m })
    end
end

-- document mapping groups
function M.map_group(modes, keys, group_name)
    for m in modes:gmatch('.') do
        wk.register({ [keys] = { name = group_name } }, { mode = m })
    end
end

-- unmap all bindings
function M.unmap(modes, keys)
    for m in modes:gmatch('.') do
        vim.api.nvim_set_keymap(m, keys, '', {})
    end
end

--------------------------------------------------------------------------------
-- Quickfix Functions
--------------------------------------------------------------------------------
local is_qf_list_open = false
local is_loc_list_open = false

function M.toggle_qf_list(is_qf)
    if is_qf then
        if is_qf_list_open then
            vim.cmd('cclose')
        else
            vim.cmd('copen')
        end
    else
        if is_loc_list_open then
            vim.cmd('lclose')
        else
            vim.cmd('lopen')
        end
    end
end

function M.set_qf_control_var()
    vim.cmd([[
        if getwininfo(win_getid())[0]['loclist'] == 1
            lua require('sanka047.core.utils').is_loc_list_open = true
        else
            lua require('sanka047.core.utils').is_qf_list_open = true
        end
    ]])
end

function M.unset_qf_control_var()
    vim.cmd([[
        if getwininfo(win_getid())[0]['loclist'] == 1
            lua require('sanka047.core.utils').is_loc_list_open = false
        else
            lua require('sanka047.core.utils').is_qf_list_open = false
        end
    ]])
end

vim.cmd([[
    augroup fixlist
        autocmd!
        autocmd BufWinEnter quickfix lua require('sanka047.core.utils').set_qf_control_var()
        autocmd BufWinLeave * lua require('sanka047.core.utils').unset_qf_control_var()
    augroup END
]])

--------------------------------------------------------------------------------
-- Miscellaneous Functions
--------------------------------------------------------------------------------
function M.split_string(input, sep)
    sep = sep or '%s'

    local t = {}
    for str in string.gmatch(input, '([^' .. sep .. ']+)') do
        table.insert(t, str)
    end
end

function M.win_move(direction)
    local curwin = vim.api.nvim_win_get_number(0)
    vim.cmd('wincmd ' .. direction)
    if curwin == vim.api.nvim_win_get_number(0) then
        if direction == 'j' or direction == 'k' then
            vim.cmd('wincmd s')
        else
            vim.cmd('wincmd v')
        end
        vim.cmd('wincmd ' .. direction)
    end
end

return M
