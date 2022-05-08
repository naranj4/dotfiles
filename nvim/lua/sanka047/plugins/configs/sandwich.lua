--------------------------------------------------------------------------------
-- Vim-Sandwich Config
--------------------------------------------------------------------------------
local M = {}

function M.setup()
    vim.cmd([[
    let g:sandwich_no_default_key_mappings = 1
    let g:operator_sandwich_no_default_key_mappings = 1
    let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)
    let g:sandwich#recipes += [
        \   {
        \       'buns'        : ['{', '}'],
        \       'motionwise'  : ['line'],
        \       'kind'        : ['add'],
        \       'linewise'    : 1,
        \       'command'     : ["'[+1,']-1normal! >>"],
        \   },
        \   {
        \       'buns'        : ['{', '}'],
        \       'motionwise'  : ['line'],
        \       'kind'        : ['delete'],
        \       'linewise'    : 1,
        \       'command'     : ["'[,']normal! <<"],
        \   },
        \   {
        \       'buns'        : ['(', ')'],
        \       'motionwise'  : ['line'],
        \       'kind'        : ['add'],
        \       'linewise'    : 1,
        \       'command'     : ["'[+1,']-1normal! >>"],
        \   },
        \   {
        \       'buns'        : ['(', ')'],
        \       'motionwise'  : ['line'],
        \       'kind'        : ['delete'],
        \       'linewise'    : 1,
        \       'command'     : ["'[,']normal! <<"],
        \   },
        \   {
        \       'buns'        : ['[', ']'],
        \       'motionwise'  : ['line'],
        \       'kind'        : ['add'],
        \       'linewise'    : 1,
        \       'command'     : ["'[+1,']-1normal! >>"],
        \   },
        \   {
        \       'buns'        : ['[', ']'],
        \       'motionwise'  : ['line'],
        \       'kind'        : ['delete'],
        \       'linewise'    : 1,
        \       'command'     : ["'[,']normal! <<"],
        \   },
        \ ]
    ]])
end

return M
