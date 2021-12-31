--------------------------------------------------------------------------------
-- Common Namespace
--------------------------------------------------------------------------------
local M = {}

function M.map(mode, key, mapping, override_opts)
    override_opts = override_opts or {}
    local opts = {noremap = true, silent = true}
    for i, v in pairs(override_opts) do
        opts[i] = v
    end

    vim.api.nvim_set_keymap(mode, key, mapping, opts)
end

function M.deepcopy(orig)
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then 
        copy = {}
        for orig_key, orig_value in next, orig, nil do
            copy[M.deepcopy(orig_key)] = M.deepcopy(orig_value)
        end
        setmetatable(copy, M.deepcopy(getmetatable(orig)))
    else  -- number, string, boolean, etc
        copy = orig
    end
    return copy
end

return M
