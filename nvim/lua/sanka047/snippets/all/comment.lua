--------------------------------------------------------------------------------
-- All Base Snippets
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
local calculate_comment_string = require('Comment.ft').calculate
local region = require('Comment.utils').get_region

--- Get the comment string {beg,end} table
---@param ctype integer 1 for `line`-comment and 2 for `block`-comment
---@return table comment_strings {begcstring, endcstring}
local function get_cstring(ctype)
    -- use the `Comments.nvim` API to fetch the comment string for the region (eq. '--%s' or '--[[%s]]' for `lua`)
    local cstring = calculate_comment_string { ctype = ctype, range = region() } or ''
    -- as we want only the strings themselves and not strings ready for using `format` we want to split the beginning and end
    local cstring_table = vim.split(cstring, '%s', { plain = true, trimempty = true })
    -- identify whether the comment-string is one or two parts and create a `{beg, end}` table for it
    if #cstring_table == 0 then
        return { '', '' } -- default
    end
    return #cstring_table == 1 and { cstring_table[1], '' } or { cstring_table[1], cstring_table[2] }
end

local marks = {
    {
        name = 'empty',
        mark = function()
            return t ''
        end,
    },
    {
        name = 'signature',
        mark = function()
            return fmt('<{}>', i(1, _G.luasnip.vars.username))
        end,
    },
    {
        name = 'date_signature',
        mark = function()
            return fmt('<{}{}>', { i(1, os.date('%d-%m-%y')), i(2, ', ' .. _G.luasnip.vars.username) })
        end,
    },
    {
        name = 'date',
        mark = function()
            return fmt('<{}>', i(1, os.date('%d-%m-%y')))
        end,
    },
}

local function todo_snippet_nodes(aliases, opts)
    local aliases_nodes = vim.tbl_map(function(alias)
        return i(nil, alias) -- generate choices for [name-of-comment]
    end, aliases)
    local sigmark_nodes = {} -- choices for [comment-mark]
    for _, mark in ipairs(marks) do
        table.insert(sigmark_nodes, mark.mark())
    end
    -- format them into the actual snippet
    local comment_node = fmt('{beg_cstr} {alias}: {text} {mark}{end_cstr}', {
        beg_cstr = f(function()
            return get_cstring(opts.ctype)[1] -- get <comment-string[1]>
        end),
        alias = c(1, aliases_nodes), -- [name-of-comment]
        text = i(3), -- {comment-text}
        mark = c(2, sigmark_nodes), -- [comment-mark]
        end_cstr = f(function()
            local end_string = get_cstring(opts.ctype)[2]
            if end_string == nil or end_string == '' then
                return ''
            end
            return ' ' .. end_string -- get <comment-string[2]>
        end),
    })
    return comment_node
end

--- Generate a TODO comment snippet with an automatic description and docstring
---@param context table merged with the generated context table `trig` must be specified
---@param aliases string[]|string of aliases for the todo comment (ex.: {FIX, ISSUE, FIXIT, BUG})
---@param opts table merged with the snippet opts table
local todo_snippet = function(context, aliases, opts)
    opts = opts or {}
    aliases = type(aliases) == 'string' and { aliases } or aliases -- if we do not have aliases, be smart about the function parameters
    context = context or {}
    if not context.trig then
        return error("context doesn't include a `trig` key which is mandatory", 2) -- all we need from the context is the trigger
    end
    opts.ctype = opts.ctype or 1 -- comment type can be passed in the `opts` table, but if it is not, we have to ensure, it is defined
    local alias_string = table.concat(aliases, '|') -- `choice_node` documentation
    context.name = context.name or (alias_string .. ' comment') -- generate the `name` of the snippet if not defined
    context.dscr = context.dscr or (alias_string .. ' comment with a signature-mark') -- generate the `dscr` if not defined
    context.docstring = context.docstring or (' {1:' .. alias_string .. '}: {3} <{2:mark}>{0} ') -- generate the `docstring` if not defined
    local comment_node = todo_snippet_nodes(aliases, opts) -- nodes from the previously defined function for their generation
    return s(context, comment_node, opts) -- the final todo-snippet constructed from our parameters
end

local todo_snippet_specs = {
    { { trig = 'todo' }, 'TODO' },
    { { trig = 'fix' }, { 'FIX', 'BUG', 'ISSUE', 'FIXIT' } },
    { { trig = 'hack' }, 'HACK' },
    { { trig = 'warn' }, { 'WARN', 'WARNING', 'XXX' } },
    { { trig = 'perf' }, { 'PERF', 'PERFORMANCE', 'OPTIM', 'OPTIMIZE' } },
    { { trig = 'note' }, { 'NOTE', 'INFO' } },
    -- NOTE: Block commented todo-comments <kunzaatko>
    { { trig = 'todob' }, 'TODO', { ctype = 2 } },
    { { trig = 'fixb' }, { 'FIX', 'BUG', 'ISSUE', 'FIXIT' }, { ctype = 2 } },
    { { trig = 'hackb' }, 'HACK', { ctype = 2 } },
    { { trig = 'warnb' }, { 'WARN', 'WARNING', 'XXX' }, { ctype = 2 } },
    { { trig = 'perfb' }, { 'PERF', 'PERFORMANCE', 'OPTIM', 'OPTIMIZE' }, { ctype = 2 } },
    { { trig = 'noteb' }, { 'NOTE', 'INFO' }, { ctype = 2 } },
}

for _, v in ipairs(todo_snippet_specs) do
    -- NOTE: 3rd argument accepts nil
    table.insert(M, todo_snippet(v[1], v[2], v[3]))
end

return M
