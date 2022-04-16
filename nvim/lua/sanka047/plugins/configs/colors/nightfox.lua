--------------------------------------------------------------------------------
-- Nightfox Config
--------------------------------------------------------------------------------
local ok, nightfox = pcall(require, 'nightfox')
if not ok then
    vim.notify('nightfox not available', 'error')
    return false
end

nightfox.setup({
    options = {
        transparent = false, -- Disable setting the background color
        dim_inactive = false, -- Non current window bg to alt color see `hl-NormalNC`
        terminal_colors = true, -- Configure the colors used when opening :terminal
        styles = {
            comments = "italic", -- Style that is applied to comments: see `highlight-args` for options
            functions = "bold", -- Style that is applied to functions: see `highlight-args` for options
            keywords = "italic", -- Style that is applied to keywords: see `highlight-args` for options
            numbers = "NONE", -- Style that is applied to numbers: see `highlight-args` for options
            strings = "NONE", -- Style that is applied to strings: see `highlight-args` for options
            types = "bold", -- Style that is applied to types: see `highlight-args` for options
            variables = "NONE", -- Style that is applied to variables: see `highlight-args` for options
        },
        inverse = {
            match_paren = false, -- Enable/Disable inverse highlighting for match parens
            visual = false, -- Enable/Disable inverse highlighting for visual selection
            search = false, -- Enable/Disable inverse highlights for search highlights
        },
    },
})
