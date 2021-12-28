--------------------------------------------------------------------------------
-- Vim-Sandwich Config
--------------------------------------------------------------------------------
local exec = vim.api.nvim_exec      -- execute vimscript

exec([[
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
]], false)
