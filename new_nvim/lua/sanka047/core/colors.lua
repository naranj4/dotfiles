--------------------------------------------------------------------------------
-- Colorscheme
--------------------------------------------------------------------------------
vim.cmd([[
    augroup HopInitHighlight
        autocmd!
        autocmd ColorScheme *
            \ highlight HopNextKey guifg=#00dfff gui=bold ctermfg=45 cterm=bold
            \ | highlight link HopNextKey1 HopNextKey
            \ | highlight HopNextKey2 guifg=#00dfff ctermfg=45
            \ | highlight HopUnmatched guifg=#666666 guibg=bg guisp=#666666 ctermfg=242
            \ | highlight link HopCursor Cursor
    augroup end
]])
