--------------------------------------------------------------------------------
-- Vim-Sandwich Config
--------------------------------------------------------------------------------
local exec = vim.api.nvim_exec      -- execute vimscript

exec([[
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
]], false)

--------------------------------------------------------------------------------
-- Vim-Sandwich Keymap
--------------------------------------------------------------------------------
local wk = require('which-key')

-- add
wk.register({ ['<leader>sa'] = { '<Plug>(operator-sandwich-add)', 'Sandwich Add' } }, { mode = '', noremap = false })

wk.register({
    -- delete
    ['<leader>sd'] = {
        '<Plug>(operator-sandwich-delete)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-query-a)',
        'Sandwich Delete',
    },
    ['<leader>sdb'] = {
        '<Plug>(operator-sandwich-delete)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-auto-a)',
        'Sandwich Delete (Auto)',
    },

    -- replace
    ['<leader>sr'] = {
        '<Plug>(operator-sandwich-replace)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-query-a)',
        'Sandwich Replace',
    },
    ['<leader>srb'] = {
        '<Plug>(operator-sandwich-replace)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-auto-a)',
        'Sandwich Replace (Auto)',
    },
}, { mode = 'n', noremap = false })

wk.register({
    -- delete
    ['<leader>sd'] = { '<Plug>(operator-sandwich-delete)', 'Sandwich Delete' },

    -- replace
    ['<leader>sr'] = { '<Plug>(operator-sandwich-replace)', 'Sandwich Replace' },
}, { mode = 'x', noremap = false })
