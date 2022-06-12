--------------------------------------------------------------------------------
-- Lua Base Snippets
--------------------------------------------------------------------------------
local ls        = require('luasnip')
local s         = ls.snippet
local sn        = ls.snippet_node
local t         = ls.text_node
local i         = ls.insert_node
local c         = ls.choice_node
local d         = ls.dynamic_node
local r         = ls.restore_node
local f         = ls.function_node
local fmt       = require('luasnip.extras.fmt').fmt
local rep       = require('luasnip.extras').rep

local M = {}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
local var_type = {
    SMART = 'smart',
    FILL = 'fill',
}

local function get_variable_name(args, parent)
    local parts = vim.split(args[1][1], '.', true)
    return sn(nil, { t(parts[#parts]) })
end

--------------------------------------------------------------------------------
-- Requires
--------------------------------------------------------------------------------
local function variable_snippet(pos, vtype, tracked_arg)
    local snip
    if vtype == var_type.FILL then
        snip = i(pos, 'var_name')
    elseif vtype == var_type.SMART then
        snip = d(pos, get_variable_name, { tracked_arg })
    end
    return snip
end

local function require_snippet(vtype)
    return sn(nil, fmt("local {var} = require('{module}')", {
        module = r(1, 'mod_name'),
        var = variable_snippet(2, vtype, 1),
    }))
end

table.insert(M, s('lre', {
    c(1, { require_snippet(var_type.SMART), require_snippet(var_type.FILL) }),
}, { stored = { mod_name = i(1, 'module') } }))

local function prequire_snippet(vtype)
    return sn(nil, fmt("local has_{status}, {value} = pcall(require, '{module}')", {
        module = r(1, 'mod_name'),
        value = variable_snippet(2, vtype, 1),
        status = rep(2),
    }))
end

-- local pcall
table.insert(M, s('lpre', {
    c(1, { prequire_snippet(var_type.SMART), prequire_snippet(var_type.FILL) })
}, { stored = { mod_name = i(1, 'module') } }))

--------------------------------------------------------------------------------
-- Pcalls
--------------------------------------------------------------------------------
-- regular pcall
table.insert(M, s('pc', fmt([[pcall({fname})]], { fname = i(1) })))

--------------------------------------------------------------------------------
-- Functions and Modules
--------------------------------------------------------------------------------
table.insert(M, s('lf', fmt([[
    local function {}({})
        {}
    end
]], { i(1, 'fname'), i(2, 'args'), i(3) })))

table.insert(M, s('mod', fmt([[
    local {module} = {table}

    {body}

    return {retval}
]], {
    table = t('{}'),
    module = i(1),
    body = i(2),
    retval = rep(1),
})))

return M
