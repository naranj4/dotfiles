--------------------------------------------------------------------------------
-- Python Base Snippets
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
-- Env
--------------------------------------------------------------------------------
table.insert(M, s('env', fmt([[
    #!/usr/bin/env python3
    {}
]], {i(0)})))

return M
