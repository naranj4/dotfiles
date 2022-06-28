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
    map('n', 'ys', 'surround', '<Plug>(operator-sandwich-add)', opts)
    map('x', 'S', 'surround', '<Plug>(operator-sandwich-add)', opts)

    -- delete
    map(
        'n',
        'ds',
        'delete surround',
        '<Plug>(operator-sandwich-delete)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-query-a)',
        opts
    )
    map(
        'n',
        'dss',
        'delete surround (auto)',
        '<Plug>(operator-sandwich-delete)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-auto-a)',
        opts
    )

    -- replace
    map(
        'n',
        'cs',
        'change surround',
        '<Plug>(operator-sandwich-replace)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-query-a)',
        opts
    )
    map(
        'n',
        'css',
        'change surround (auto)',
        '<Plug>(operator-sandwich-replace)<Plug>(operator-sandwich-release-count)<Plug>(textobj-sandwich-auto-a)',
        opts
    )

    -- textobjects
    map('xo', 'is', 'in sandwich', '<Plug>(textobj-sandwich-auto-i)')
    map('xo', 'is', 'around sandwich', '<Plug>(textobj-sandwich-auto-i)')
end

return M
