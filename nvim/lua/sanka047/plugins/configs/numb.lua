--------------------------------------------------------------------------------
-- Numb Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local ok, numb = pcall(require, 'numb')
if not ok then
    log.error('numb not available', 'Config')
    return false
end

numb.setup({
    show_numbers = true,
    show_cursorline = true,
    number_only = false,
    centered_peeking = true,
})
