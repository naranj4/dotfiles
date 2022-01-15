--------------------------------------------------------------------------------
-- OneDarkPro Config
--------------------------------------------------------------------------------
local ok, onedarkpro = pcall(require, 'onedarkpro')
if not ok then
    print('onedarkpro not available')
    return false
end

onedarkpro.setup({
    theme = 'onedark',
    colors = {},
    hlgroups = {},
    plugins = {
        native_lsp = true,
        polygot = false,
        treesitter = true,
    },
    styles = {
        strings = 'NONE',
        comments = 'italic',
        keywords = 'italic',
        functions = 'NONE',
        variables = 'bold',
    },
    options = {
        bold = true,
        italic = true,
        underline = true,
        undercurl = false,
        cursorline = true,
        transparency = false,
        terminal_colors = true,
        window_unfocused_color = false,
    },
})
