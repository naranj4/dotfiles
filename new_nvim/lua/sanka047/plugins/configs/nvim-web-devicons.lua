--------------------------------------------------------------------------------
-- Web Devicons Config
--------------------------------------------------------------------------------
local ok, web_devicons = pcall(require, 'nvim-web-devicons')
if not ok then
    print('nvim-web-devicons not available')
    return false
end

web_devicons.setup({
    default = true;
})
