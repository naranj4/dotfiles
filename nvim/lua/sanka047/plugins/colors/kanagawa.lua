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

        FloatBorder = { fg = colors.bg_dark, bg = colors.bg },
        FloatTitle = { fg = colors.bg_dark, bg = colors.crystalBlue, bold = true },

        CmpCompletionBorder = { fg = colors.bg_menu, bg = colors.bg },
        CmpDocumentation = { bg = colors.bg_dim },
        CmpDocumentationBorder = { fg = colors.bg_dim, bg = colors.bg },

        QuickScopePrimary = { fg = colors.crystalBlue, underline = true, bold = true },
        QuickScopeSecondary = { fg = colors.diag.warning, underline = true, bold = true },

        LeapMatch = { link = 'QuickScopeSecondary' },
        LeapLabelPrimary = { fg = colors.bg_dark, bg = colors.carpYellow, bold = true },
        LeapLabelSecondary = { fg = colors.bg_dark, bg = colors.crystalBlue, bold = true },
        LeapBackdrop = { fg = colors.fg_comment, bg = colors.bg },

        -- TelescopeNormal = { fg = colors.fg_dark, bg = colors.winterBlue },
        -- TelescopeBorder = { fg = colors.winterBlue, bg = colors.bg },
        -- TelescopeTitle = { fg = colors.winterBlue, bg = colors.crystalBlue, bold = true },
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
