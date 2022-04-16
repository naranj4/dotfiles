--------------------------------------------------------------------------------
-- Colorizer Config
--------------------------------------------------------------------------------
local ok, colorizer = pcall(require, 'colorizer')
if not ok then
    vim.notify('colorizer not available', 'error')
    return false
end

colorizer.setup({}, { mode = 'background' })
