--------------------------------------------------------------------------------
-- Dressing Config
--------------------------------------------------------------------------------
local ok, dressing = pcall(require, 'dressing')
if not ok then
    print('dressing not available')
    return false
end

dressing.setup({
    input = {
        enabled = true,
        insert_only = true,
    },
    select = {
        enabled = true,
        backend = { "telescope", "builtin" },
    },
})
