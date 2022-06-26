--------------------------------------------------------------------------------
-- Vim-Sandwich Config
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

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

--------------------------------------------------------------------------------
-- Sandwich Keymap
--------------------------------------------------------------------------------
function M.keymap()
    local opts = { noremap = false }

    -- add
    map('', '<leader>sa', 'Sandwich Add', '<Plug>(operator-sandwich-add)', opts)

    -- delete
    map(
        'nx',
        '<leader>sd',
        'Sandwich Delete',
        '<Plug>(operator-sandwich-delete)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-query-a)',
        opts
    )
    map(
        'n',
        '<leader>sdb',
        'Sandwich Delete (Auto)',
        '<Plug>(operator-sandwich-delete)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-auto-a)',
        opts
    )

    -- replace
    map(
        'nx',
        '<leader>sr',
        'Sandwich Replace',
        '<Plug>(operator-sandwich-replace)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-query-a)',
        opts
    )
    map(
        'n',
        '<leader>srb',
        'Sandwich Replace (Auto)',
        '<Plug>(operator-sandwich-replace)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-auto-a)',
        opts
    )
end

return M
