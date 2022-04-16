--------------------------------------------------------------------------------
-- Numb Config
--------------------------------------------------------------------------------
local ok, numb = pcall(require, 'numb')
if not ok then
    vim.notify('numb not available', 'error')
    return false
end

numb.setup({
    show_numbers = true,
    show_cursorline = true,
    number_only = false,
    centered_peeking = true,
})
