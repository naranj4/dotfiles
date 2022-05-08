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

return M
