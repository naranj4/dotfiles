--------------------------------------------------------------------------------
-- FTerm Config
--------------------------------------------------------------------------------
local ok, fterm = pcall(require, 'FTerm')
if not ok then
    vim.notify('FTerm not available', 'error')
    return false
end

fterm.setup({
    border = 'rounded',
    blend = 20,
    dimensions = {
        height = 0.9,
        width = 0.9,
    },
})
