--------------------------------------------------------------------------------
-- Web Devicons Config
--------------------------------------------------------------------------------
local ok, web_devicons = pcall(require, 'nvim-web-devicons')
if not ok then
    vim.notify('nvim-web-devicons not available', 'error')
    return false
end

web_devicons.setup({
    default = true;
})
