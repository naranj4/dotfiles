--------------------------------------------------------------------------------
-- Hop Config
--------------------------------------------------------------------------------
local ok, hop = pcall(require, 'hop')
if not ok then
    vim.notify('hop not available', 'error')
    return false
end

hop.setup({
    create_hl_autocmd = false,
})
