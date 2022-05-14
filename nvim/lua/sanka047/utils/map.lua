--------------------------------------------------------------------------------
-- Mapping Functions
--------------------------------------------------------------------------------
local M = {}

local wk = require('which-key')
local merge_opts = require('sanka047.utils').merge_opts

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

-- create autocmd group
function M.create_augroup(name, override_opts)
    local opts = merge_opts({}, override_opts)

    vim.api.nvim_create_augroup(name, opts)
end

-- create auto command
function M.create_autocmd(event, desc, override_opts)
    local opts = merge_opts({ desc = desc }, override_opts)

    vim.api.nvim_create_autocmd(event, opts)
end

return M
