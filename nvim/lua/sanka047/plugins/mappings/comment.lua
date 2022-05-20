--------------------------------------------------------------------------------
-- Comment Keymap Documentation
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

local M = {}

function M.keymap()
    -- Line comments
    map(
        'n',
        '<leader>c',
        'Line Comment',
        '<CMD>lua require("Comment.api").call("toggle_linewise_op")<CR>g@'
    )
    map(
        'x',
        '<leader>c',
        'Line Comment',
        '<ESC><CMD>lua require("Comment.api").toggle_linewise_op(vim.fn.visualmode())<CR>'
    )

    map(
        'n',
        '<leader>cc',
        'Toggle Line Comment',
        '<CMD>lua require("Comment.api").toggle_current_linewise()<CR>'
    )

    -- Block comments
    map(
        'n',
        '<leader>b',
        'Block Comment',
        '<CMD>lua require("Comment.api").call("toggle_blockwise_op")<CR>g@'
    )
    map(
        'x',
        '<leader>b',
        'Block Comment',
        '<ESC><CMD>lua require("Comment.api").toggle_blockwise_op(vim.fn.visualmode())<CR>'
    )
    map(
        'n',
        '<leader>bc',
        'Toggle Block Comment',
        '<CMD>lua require("Comment.api").toggle_current_blockwise()<CR>'
    )

    -- Extra mappings
    map(
        'n',
        '<leader>co',
        'Add Comment Above',
        '<CMD>lua require("Comment.api").insert_linewise_below()<CR>'
    )
    map(
        'n',
        '<leader>cO',
        'Add Comment Below',
        '<CMD>lua require("Comment.api").insert_linewise_above()<CR>'
    )
    map(
        'n',
        '<leader>cA',
        'Add Comment (EOL)',
        '<CMD>lua require("Comment.api").insert_linewise_eol()<CR>'
    )
end

return M
