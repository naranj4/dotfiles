--------------------------------------------------------------------------------
-- Mason LSP Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local has_mason_lspconfig, mason_lspconfig = pcall(require, 'mason-lspconfig')
    if not has_mason_lspconfig then
        log.error('mason-lspconfig not available', 'Config')
        return false
    end

    mason_lspconfig.setup()
end

return M
