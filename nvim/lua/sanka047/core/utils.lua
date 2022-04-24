--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------
local wk = require('which-key')

local M = {}

local function merge_opts(opts, override)
    override = override or {}
    for i, v in pairs(override) do
        opts[i] = v
    end
    return opts
end

--------------------------------------------------------------------------------
-- Config Functions
--------------------------------------------------------------------------------
function M.reload_config()
    RELOAD('sanka047')
    vim.cmd([[luafile ~/.config/nvim/init.lua]])
end

--------------------------------------------------------------------------------
-- Mapping Functions
--------------------------------------------------------------------------------

-- map keybind for all specified modes
function M.map(modes, keys, desc, mapping, override_opts, which_key_opts)
    if modes == '' then
        modes = 'nvo'
    end

    local opts = merge_opts({ silent = true, desc = desc }, override_opts)
    local w_opts = merge_opts({}, which_key_opts)

    -- convert modes string to a list
    local m_list = {}
    for m in modes:gmatch('.') do
        table.insert(m_list, m)

        -- Document the keymap using which keys immediately after mapping. wk has seemingly been
        -- pretty buggy with mapping for me, so checking to see if it will still work for
        -- documenting keymaps.
        w_opts['mode'] = m
        wk.register({ [keys] = desc }, w_opts)
    end
    if mapping ~= nil then
        vim.keymap.set(m_list, keys, mapping, opts)
    end
end

-- document mapping groups
function M.map_group(modes, keys, group_name)
    for m in modes:gmatch('.') do
        wk.register({ [keys] = { name = group_name } }, { mode = m })
    end
end

-- unmap all bindings
function M.unmap(modes, keys, override_opts)
    local opts = merge_opts({}, override_opts)

    local m_list = {}
    for m in modes:gmatch('.') do
        table.insert(m_list, m)
    end
    vim.keymap.del(m_list, keys, opts)
end

-- buffer specific mapping
function M.buf_map(bufnr, modes, keys, desc, mapping, override_opts, which_key_opts)
    local opts = merge_opts({ buffer = bufnr }, override_opts)
    local w_opts = merge_opts({ buffer = bufnr }, which_key_opts)
    M.map(modes, keys, desc, mapping, opts, w_opts)
end

-- document mapping groups
function M.buf_map_group(bufnr, modes, keys, group_name)
    for m in modes:gmatch('.') do
        wk.register({ [keys] = { name = group_name } }, { mode = m, buffer = bufnr })
    end
end

-- unmap all bindings
function M.buf_unmap(bufnr, modes, keys, override_opts)
    local opts = merge_opts({ buffer = bufnr }, override_opts)
    M.unmap(modes, keys, opts)
end

-- create user command
function M.create_command(name, cmd, desc, override_opts)
    local opts = merge_opts({ desc = desc }, override_opts)

    vim.api.nvim_create_user_command(name, cmd, opts)
end

--------------------------------------------------------------------------------
-- Quickfix Functions
--------------------------------------------------------------------------------
M.is_qf_list_open = false
M.is_loc_list_open = false

function M.toggle_qf_list(is_qf)
    if is_qf then
        if M.is_qf_list_open then
            vim.cmd('cclose')
        else
            vim.cmd('copen')
        end
    else
        if M.is_loc_list_open then
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
