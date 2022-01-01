--------------------------------------------------------------------------------
-- Hop Config
--------------------------------------------------------------------------------
local exec = vim.api.nvim_exec

require('hop').setup({
    create_hl_autocmd = false,
})

exec([[
    augroup HopInitHighlight
        autocmd!
        autocmd ColorScheme *
            \ highlight HopNextKey guifg=#00dfff gui=bold ctermfg=45 cterm=bold
            \ | highlight link HopNextKey1 HopNextKey
            \ | highlight HopNextKey2 guifg=#00dfff ctermfg=45
            \ | highlight HopUnmatched guifg=#666666 guibg=bg guisp=#666666 ctermfg=242
            \ | highlight link HopCursor Cursor
    augroup end
]], false)

--------------------------------------------------------------------------------
-- Hop Keymap
--------------------------------------------------------------------------------
local wk = require('which-key')

wk.register({
    ['<leader>j'] = {
        '<CMD>lua require("hop").hint_lines({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
        'Hop Down',
    },
    ['<leader>k'] = {
        '<CMD>lua require("hop").hint_lines({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
        'Hop Up',
    },
    ['<leader>w'] = { '<CMD>lua require("hop").hint_words()<CR>', 'Hop Word' },
}, { mode = '' })
