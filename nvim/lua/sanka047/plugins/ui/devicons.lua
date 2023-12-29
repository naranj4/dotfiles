--------------------------------------------------------------------------------
-- Web Devicons Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, web_devicons = pcall(require, 'nvim-web-devicons')
    if not ok then
        log.error('nvim-web-devicons not available', 'Config')
        return false
    end

    web_devicons.setup({ default = true })
end

return M
