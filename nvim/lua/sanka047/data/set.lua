--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------
local Set = {}
Set.__index = Set

-- Create a new Set object
function Set.new(t)
    t = t or {}
    local s = {}
    setmetatable(s, Set)
    for _, v in ipairs(t) do s[v] = true end
    return s
end

-- Get string representation of a set
function Set.tostring(set)
    local s = '{ '
    local sep = ''
    for e in pairs(set) do
        s = s .. sep .. e
        sep = ', '
    end
    return s .. ' }'
end

-- Set union
function Set.union(a, b)
    local res = Set.new()
    for k in pairs(a) do res[k] = true end
    for k in pairs(b) do res[k] = true end
    return res
end

-- Set intersection
function Set.intersection(a, b)
    local res = Set.new()

    local small, large = a, b
    if table.getn(b) < table.getn(a) then
        small, large = b, a
    end

    for k in pairs(small) do
        res[k] = large[k]
    end

    return res
end

-- Set complement
function Set.complement(a, b)
    local res = Set.new()
    for k in pairs(a) do
        if not b[k] then
            res[k] = true
        end
    end
    return res
end

-- Set concatenation
function Set.concatenate(set, item)
    local b = Set.new({item})
    return set + b
end

-- Set insertion (in-place)
function Set.insert(set, item)
    set[item] = true
end

Set.__tostring = Set.tostring
Set.__add = Set.union
Set.__mul = Set.intersection
Set.__sub = Set.complement
Set.__concat = Set.concatenate

return Set
