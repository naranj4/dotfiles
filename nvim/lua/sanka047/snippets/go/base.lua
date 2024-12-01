--------------------------------------------------------------------------------
-- Go Snippets
--
-- NOTE: It's important to actually use tabs for indenting in the snippet strings here or it will
-- result in formatting issues with golang
--------------------------------------------------------------------------------
local ls        = require('luasnip')
local s         = ls.snippet
local sn        = ls.snippet_node
local isn       = ls.indent_snippet_node
local t         = ls.text_node
local i         = ls.insert_node
local f         = ls.function_node
local c         = ls.choice_node
local d         = ls.dynamic_node
local r         = ls.restore_node
local fmt       = require('luasnip.extras.fmt').fmt

local M = {}

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------
table.insert(M, s('iferr', fmt(
    [[
        if err != nil {{
        	{ret}
        }}
    ]], {
        ret = c(1, {
            sn(nil, fmt('return {val}, err', { val = i(1, 'val') })), -- return val and err
            t('return err'), -- return err
            i(1), -- custom return
        }),
    }
)))

return M
