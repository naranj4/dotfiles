--------------------------------------------------------------------------------
-- Nvim-Autopairs Config
--------------------------------------------------------------------------------
local npairs = require('nvim-autopairs')

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
    map_cr = false,

    fast_wrap = {},
})
