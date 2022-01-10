--------------------------------------------------------------------------------
-- Hop Config
--------------------------------------------------------------------------------
local ok, hop = pcall(require, 'hop')
if not ok then
    print('hop not available')
    return false
end

hop.setup({
    create_hl_autocmd = false,
})
