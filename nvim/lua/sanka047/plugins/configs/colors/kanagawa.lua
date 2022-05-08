--------------------------------------------------------------------------------
-- Kanagawa Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, kanagawa = pcall(require, 'kanagawa')
    if not ok then
        log.error('kanagawa not available', 'Config')
        return false
    end

    vim.g.kanagawa_lualine_bold = true

    kanagawa.setup({
        undercurl = true,
        commentStyle = 'italic',
        functionStyle = 'bold',
        keywordStyle = 'italic',
        statementStyle = 'bold',
        typeStyle = 'bold',
        variablebuiltinStyle = 'italic',
        specialReturn = true,
        specialException = true,
        transparent = false,
        dimInactive = false, -- while shade is broken
        globalStatus = true,
        colors = {},
        overrides = {},
    })

    vim.cmd('colorscheme kanagawa')
end

return M
