--------------------------------------------------------------------------------
-- Python Function Snippets
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
-- Helpers
--------------------------------------------------------------------------------
local function clean_param(raw_param)
    return vim.trim(vim.split(raw_param, '%s*=%s*', { trimempty = true })[1])
end

local function generate_doc()
    return isn(nil, {
        c(1, {
            t(''), -- no docstring
            sn(nil, fmt([[
            """{desc}"""

            ]], { desc = i(1) })), -- one line docstring
        })
    }, '$PARENT_INDENT\t')
end

--------------------------------------------------------------------------------
-- def
--------------------------------------------------------------------------------
table.insert(M, s('df', fmt(
    [[
        def {func}({args}){rtype}:
            {doc}{body}
    ]], {
        func = i(1, 'fname'),
        args = i(2, 'args'),
        rtype = c(3, {
            t(''), -- no rtype
            sn(nil, fmt('{sep}-> {rtype}', { sep = t(' '), rtype = i(1, 'None') })), -- rtype
        }),
        doc = c(4, {
            t(''), -- no docstring
            isn(nil, fmt([[
                """{desc}"""

            ]], { desc = i(1) }), '$PARENT_INDENT\t'), -- one line docstring
        }),
        body = i(5),
    }
)))

return M
