--------------------------------------------------------------------------------
-- Substitute Config
--------------------------------------------------------------------------------
local ok, substitute = pcall(require, 'substitute')
if not ok then
    vim.notify('substitute not available', 'error')
    return false
end

substitute.setup({
    yank_substitued_text = false,
    range = {
        prefix = 'S',
    },
})
