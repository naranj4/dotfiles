--------------------------------------------------------------------------------
-- Nvim-Comment Config
--------------------------------------------------------------------------------
require('nvim_comment').setup({
    marker_padding = true,
    comment_empty = true,
    create_mappings = true,
    line_mapping = '<leader>cc',
    operator_mapping = '<leader>c',
    hook = nil,
})
