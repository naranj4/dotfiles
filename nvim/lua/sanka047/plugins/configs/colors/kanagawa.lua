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

    -- Overrides
    local colors = require('kanagawa.colors').setup()
    local overrides = {
        InclineNormal = { bg = colors.fn, fg = colors.bg_dark, bold = true },
        InclineNormalNC = { bg = colors.bg_light0, fg = colors.fg_dark },
        HopNextKey = { fg = colors.fg, bold = true },
        HopNextKey1 = { link = 'HopNextKey' },
        HopNextKey2 = { fg = colors.fg },
        HopUnmatched = { bg = colors.bg, fg = colors.fg_comment },
        HopCursor = { link = 'Cursor' },
    }

    kanagawa.setup({
        undercurl = true,
        commentStyle = { italic = true },
        functionStyle = { bold = true },
        keywordStyle = { bold = true },
        statementStyle = { bold = true },
        typeStyle = { bold = true },
        variablebuiltinStyle = { bold = true },
        specialReturn = true,
        specialException = true,
        transparent = false,
        dimInactive = false, -- while shade is broken
        globalStatus = true,
        colors = {},
        overrides = overrides,
    })

    vim.cmd('colorscheme kanagawa')
end

return M
