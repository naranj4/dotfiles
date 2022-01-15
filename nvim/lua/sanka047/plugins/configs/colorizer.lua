--------------------------------------------------------------------------------
-- Colorizer Config
--------------------------------------------------------------------------------
local ok, colorizer = pcall(require, 'colorizer')
if not ok then
    print('colorizer not available')
    return false
end

colorizer.setup({}, { mode = 'background' })
