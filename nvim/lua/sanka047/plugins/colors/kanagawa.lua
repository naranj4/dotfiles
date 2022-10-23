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
        FloatTitle = { fg = colors.bg_dark, bg = colors.waveAqua2, bold = true },

        DressingNormal = { fg = colors.fg, bg = colors.winterBlue },
        DressingBorder = { fg = colors.winterBlue, bg = colors.bg },

        CmpCompletionBorder = { fg = colors.bg_menu, bg = colors.bg },
        CmpDocumentation = { bg = colors.bg_dim },
        CmpDocumentationBorder = { fg = colors.bg_dim, bg = colors.bg },

        QuickScopePrimary = { fg = colors.crystalBlue, underline = true, bold = true },
        QuickScopeSecondary = { fg = colors.diag.warning, underline = true, bold = true },

        LeapMatch = { link = 'QuickScopeSecondary' },
        LeapLabelPrimary = { fg = colors.bg_dark, bg = colors.carpYellow, bold = true },
        LeapLabelSecondary = { fg = colors.bg_dark, bg = colors.crystalBlue, bold = true },
        LeapBackdrop = { fg = colors.fg_comment, bg = colors.bg },

        ToggletermNormal = { fg = colors.fg, bg = colors.bg },
        ToggletermBorder = { fg = colors.fg_border, bg = colors.bg },

        TelescopeNormal = { fg = colors.fg_dark, bg = colors.winterBlue },
        TelescopeBorder = { fg = colors.winterBlue, bg = colors.winterBlue },
        TelescopeTitle = { fg = colors.winterBlue, bg = colors.crystalBlue, bold = true },

        TelescopePromptNormal = { fg = colors.fg_dark, bg = colors.bg_menu },
        TelescopePromptBorder = { fg = colors.bg_menu, bg = colors.bg_menu },
        TelescopePromptTitle = { fg = colors.bg, bg = colors.autumnYellow, bold = true },

        TelescopeResultsTitle = { fg = colors.winterBlue, bg = colors.winterBlue, bold = true },

        TelescopePreviewTitle = { fg = colors.bg, bg = colors.boatYellow2, bold = true },

        CmpItemAbbrDeprecated = { fg = colors.katanaGray, bg = "NONE", strikethrough = true },
        CmpItemAbbrMatch = { fg = colors.crystalBlue, bg = "NONE", bold = true },
        CmpItemAbbrMatchFuzzy = { fg = colors.crystalBlue, bg = "NONE", bold = true },

        CmpItemKindField = { fg = colors.bg_menu, bg = colors.autumnGreen },
        CmpItemKindProperty = { fg = colors.bg_menu, bg = colors.autumnGreen },
        CmpItemKindEvent = { fg = colors.bg_menu, bg = colors.autumnGreen },

        CmpItemKindText = { fg = colors.bg_menu, bg = colors.springGreen },
        CmpItemKindEnum = { fg = colors.bg_menu, bg = colors.carpYellow },
        CmpItemKindKeyword = { fg = colors.bg_menu, bg = colors.oniViolet },

        CmpItemKindConstant = { fg = colors.bg_menu, bg = colors.surimiOrange },
        CmpItemKindConstructor = { fg = colors.bg_menu, bg = colors.surimiOrange },
        CmpItemKindReference = { fg = colors.bg_menu, bg = colors.surimiOrange },

        CmpItemKindFunction = { fg = colors.bg_menu, bg = colors.crystalBlue },
        CmpItemKindStruct = { fg = colors.bg_menu, bg = colors.waveAqua2 },
        CmpItemKindClass = { fg = colors.bg_menu, bg = colors.waveAqua2 },

        CmpItemKindModule = { fg = colors.bg_menu, bg = colors.surimiOrange },
        CmpItemKindOperator = { fg = colors.carpYellow, bg = colors.boatYellow2 },

        CmpItemKindVariable = { fg = colors.bg_menu, bg = colors.carpYellow },
        CmpItemKindFile = { fg = colors.bg_menu, bg = colors.carpYellow },

        CmpItemKindUnit = { fg = colors.bg_menu, bg = colors.carpYellow },
        CmpItemKindSnippet = { fg = colors.bg_menu, bg = colors.carpYellow },
        CmpItemKindFolder = { fg = colors.bg_menu, bg = colors.carpYellow },

        CmpItemKindMethod = { fg = colors.bg_menu, bg = colors.crystalBlue },
        CmpItemKindValue = { fg = colors.bg_menu, bg = colors.crystalBlue },
        CmpItemKindEnumMember = { fg = colors.bg_menu, bg = colors.crystalBlue },

        CmpItemKindInterface = { fg = colors.bg_menu, bg = colors.sakuraPink },
        CmpItemKindColor = { fg = colors.bg_menu, bg = colors.sakuraPink },
        CmpItemKindTypeParameter = { fg = colors.bg_menu, bg = colors.sakuraPink },
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
