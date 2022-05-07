--------------------------------------------------------------------------------
-- Tabout Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local ok, tabout = pcall(require, 'tabout')
if not ok then
    log.error('tabout not available', 'Config')
    return false
end

tabout.setup({
    tabkey = '<C-L>', -- key to trigger tabout
    backwards_tabkey = '<C-H>',
    act_as_tab = false,
    act_as_shift_tab = false,
    enable_backwards = true,
    completion = false,
    tabouts = {
        {open = '"', close = '"'},
        {open = "'", close = "'"},
        {open = '`', close = '`'},
        {open = '(', close = ')'},
        {open = '[', close = ']'},
        {open = '{', close = '}'},
    },
    ignore_beginning = true,
})
