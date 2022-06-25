--------------------------------------------------------------------------------
-- LSP Installer Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local has_lsp_installer, lsp_installer = pcall(require, 'nvim-lsp-installer')
    if not has_lsp_installer then
        log.error('nvim-lsp-installer not available', 'Config')
        return false
    end

    lsp_installer.setup({
        ui = {
            icons = {
                server_installed = "✓",
                server_pending = "➜",
                server_uninstalled = "✗",
            },
        },
    })
end

return M
