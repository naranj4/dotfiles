--------------------------------------------------------------------------------
-- Nvim-Autopairs Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, npairs = pcall(require, 'nvim-autopairs')
    if not ok then
        log.error('nvim-autopairs not available', 'Config')
        return false
    end

    -- coq_nvim
    -- npairs.setup({
    --     disable_filetype = {'TelescopePrompt'},
    --     map_bs = false,
    --     map_cr = false,

    --     fast_wrap = {},
    -- })

    -- nvim-cmp
    npairs.setup({
        disable_filetype = {'TelescopePrompt'},
        map_bs = true,
        map_cr = true,

        fast_wrap = {},
        check_ts = true,
    })
end

--------------------------------------------------------------------------------
-- Nvim-Autopairs Keymap
--------------------------------------------------------------------------------
function M.keymap()
-- function _G.MUtils.CR()
--     if vim.fn.pumvisible() ~= 0 then
--         if vim.fn.complete_info({ 'selected' }).selected ~= -1 then
--             return npairs.esc('<c-y>')
--         else
--             return npairs.esc('<c-e>') .. npairs.autopairs_cr()
--         end
--     else
--         return npairs.autopairs_cr()
--     end
-- end

-- function _G.MUtils.BS()
--     if vim.fn.pumvisible() ~= 0 and vim.fn.complete_info({ 'mode' }).mode == 'eval' then
--         return npairs.esc('<c-e>') .. npairs.autopairs_bs()
--     else
--         return npairs.autopairs_bs()
--     end
-- end

-- wk.register({
--     ['<CR>'] = { 'v:lua.MUtils.CR()', '<CR>' },
--     ['<BS>'] = { 'v:lua.MUtils.BS()', '<BS>' },
-- }, { mode = 'i' })
end

return M
