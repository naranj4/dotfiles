--------------------------------------------------------------------------------
-- Numb Config
--------------------------------------------------------------------------------
local ok, numb = pcall(require, 'numb')
if not ok then
    print('numb not available')
    return false
end

numb.setup({
    show_numbers = true,
    show_cursorline = true,
    number_only = false,
    centered_peeking = true,
})
