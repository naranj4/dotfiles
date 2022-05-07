--------------------------------------------------------------------------------
-- Substitute Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local ok, substitute = pcall(require, 'substitute')
if not ok then
    log.error('substitute not available', 'Config')
    return false
end

substitute.setup({
    yank_substitued_text = false,
    range = {
        prefix = 'S',
    },
})
