--------------------------------------------------------------------------------
-- Indent-Blankline Config
--------------------------------------------------------------------------------
local ok, indent_blankline = pcall(require, 'indent_blankline')
if not ok then
    vim.notify('indent-blankline not available', 'error')
    return false
end

indent_blankline.setup({
    space_char_blankline = ' ',
    show_current_context = true,
    show_current_context_start = false,
    show_end_of_line = false,
    buftype_exclude = { 'terminal', 'prompt', 'nofile' },
    filetype_exclude = { 'dashboard', 'packer', 'lspinfo' },
})
