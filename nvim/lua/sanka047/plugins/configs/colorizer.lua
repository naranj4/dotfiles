--------------------------------------------------------------------------------
-- Colorizer Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local ok, colorizer = pcall(require, 'colorizer')
if not ok then
    log.error('colorizer not available', 'Config')
    return false
end

colorizer.setup({}, { mode = 'background' })
