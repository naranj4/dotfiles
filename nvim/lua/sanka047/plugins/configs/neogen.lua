--------------------------------------------------------------------------------
-- Neogen Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, neogen = pcall(require, 'neogen')
    if not ok then
        log.error('neogen not available', 'Config')
        return false
    end

    neogen.setup({
        enabled = true,
        snippet_engine = 'luasnip',
        languages = {
            python = { template = { annotation_convention = 'numpydoc' } },
        },
    })
end

return M
