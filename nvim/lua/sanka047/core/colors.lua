--------------------------------------------------------------------------------
-- Colorscheme
--------------------------------------------------------------------------------
local create_augroup = require('sanka047.utils.map').create_augroup
local create_autocmd = require('sanka047.utils.map').create_autocmd

--------------------------------------------------------------------------------
-- Incline highlighting
--------------------------------------------------------------------------------
create_augroup('InclineHighlight')
create_autocmd(
    'ColorScheme',
    'Sets Incline highlighting on colorscheme change',
    {
        group = 'InclineHighlight',
        pattern = 'kanagawa',
        callback = function ()
            local set_hl = function (...) vim.api.nvim_set_hl(0, ...) end
            local colors = require('kanagawa.colors').setup()

            set_hl(
                'InclineNormal',
                { bg = colors.crystalBlue, fg = colors.bg_dark, bold = 1 }
            )
            set_hl(
                'InclineNormalNC',
                { bg = colors.bg_light0, fg = colors.crystalBlue, italic = 1 }
            )
        end
    }
)

--------------------------------------------------------------------------------
-- Hop highlighting
--------------------------------------------------------------------------------
create_augroup('HopInitHighlight')
create_autocmd(
    'ColorScheme',
    'Sets Hop highlighting on colorscheme change',
    {
        group = 'HopInitHighlight',
        pattern = '*',
        callback = function ()
            vim.cmd([[highlight HopNextKey guifg=#00dfff gui=bold ctermfg=45 cterm=bold]])
            vim.cmd([[highlight link HopNextKey1 HopNextKey]])
            vim.cmd([[highlight HopNextKey2 guifg=#00dfff ctermfg=45]])
            vim.cmd([[highlight HopUnmatched guifg=#666666 guibg=bg guisp=#666666 ctermfg=242]])
            vim.cmd([[highlight link HopCursor Cursor]])
        end
    }
)
