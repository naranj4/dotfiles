--------------------------------------------------------------------------------
-- Dressing Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local ok, dressing = pcall(require, 'dressing')
if not ok then
    log.error('dressing not available', 'Config')
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
