--------------------------------------------------------------------------------
-- Treesitter-Context Config
--------------------------------------------------------------------------------
local ok, treesitter_context = pcall(require, 'treesitter-context')
if not ok then
    print('nvim-treesitter-context not available')
    return false
end

treesitter_context.setup({
    enable = true,
    throttle = true,
    max_lines = 1,
    patterns = {
        default = {
            'class',
            'function',
            'method',
        },
    },
})
