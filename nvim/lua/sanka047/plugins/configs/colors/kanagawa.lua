--------------------------------------------------------------------------------
-- Kanagawa Config
--------------------------------------------------------------------------------
local ok, kanagawa = pcall(require, 'kanagawa')
if not ok then
    print('kanagawa not available')
    return false
end

vim.g.kanagawa_lualine_bold = true

kanagawa.setup({
    undercurl = false,
    commentStyle = 'italic',
    functionStyle = 'bold',
    keywordStyle = 'italic',
    statementStyle = 'bold',
    typeStyle = 'bold',
    variablebuiltinStyle = 'italic',
    specialReturn = true,
    specialException = true,
    transparent = false,
    dimInactive = true, -- while shade is broken
    colors = {},
    overrides = {},
})

vim.cmd('colorscheme kanagawa')
