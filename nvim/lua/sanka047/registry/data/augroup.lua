--------------------------------------------------------------------------------
-- Augroup
--------------------------------------------------------------------------------
local AuGroup = {}
AuGroup.__index = AuGroup

-- Create a new AuGroup object
-- @param name string: auto group name
-- @return AuGroup
function AuGroup.new(name)
    local group = {}
    setmetatable(group, AuGroup)

    group.name = name or ''

    -- create an augroup
    group.id = vim.api.nvim_create_augroup(name, { clear = true })

    return group
end

-- Removes and clean up the autocmd
-- @param group AuGroup: the autocmd to clean up
function AuGroup.remove(group)
    vim.api.nvim_del_augroup_by_id(group.id)
end

-- String representation of an augroup
-- @param group AuGroup
-- @return string
function AuGroup.tostring(group)
    local s = (
        'augroup(' .. group.id .. ')' .. ': ' .. group.name
    )
    return s
end

return AuGroup
