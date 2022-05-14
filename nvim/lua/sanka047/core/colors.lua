--------------------------------------------------------------------------------
-- Colorscheme
--------------------------------------------------------------------------------
local create_augroup = require('sanka047.utils.map').create_augroup
local create_autocmd = require('sanka047.utils.map').create_autocmd

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
