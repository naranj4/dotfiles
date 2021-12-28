--------------------------------------------------------------------------------
-- Nvim-Autopairs Config
--------------------------------------------------------------------------------
local map = require('common').map
local npairs = require('nvim-autopairs')

npairs.setup({
    disable_filetype = {'TelescopePrompt'},
    map_bs = false,
    map_cr = false,

    fast_wrap = {},
})

--------------------------------------------------------------------------------
-- Nvim-Autopairs Keymap (to work with coq_nvim)
--------------------------------------------------------------------------------
function _G.MUtils.CR()
    if vim.fn.pumvisible() ~= 0 then
        if vim.fn.complete_info({ 'selected' }).selected ~= -1 then
            return npairs.esc('<c-y>')
        else
            return npairs.esc('<c-e>') .. npairs.autopairs_cr()
        end
    else
        return npairs.autopairs_cr()
    end
end

map('i', '<CR>', 'v:lua.MUtils.CR()', { expr = true })

function _G.MUtils.BS()
    if vim.fn.pumvisible() ~= 0 and vim.fn.complete_info({ 'mode' }).mode == 'eval' then
        return npairs.esc('<c-e>') .. npairs.autopairs_bs()
    else
        return npairs.autopairs_bs()
    end
end

map('i', '<BS>', 'v:lua.MUtils.BS()', { expr = true })
