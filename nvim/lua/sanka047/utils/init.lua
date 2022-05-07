--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------
local M = {}

function M.split_string(input, sep)
    sep = sep or '%s'

    local t = {}
    for str in string.gmatch(input, '([^' .. sep .. ']+)') do
        table.insert(t, str)
    end
end

function M.merge_opts(opts, override)
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

return M
