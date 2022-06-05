--------------------------------------------------------------------------------
-- LuaSnip Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, luasnip = pcall(require, 'luasnip')
    if not ok then
        log.error('luasnip not available', 'Config')
        return false
    end

    luasnip.config.setup({
        history = true,
        update_events = 'TextChanged,TextChangedI',
    })

    -- add snippet folders here
    local snip_paths = {}

    local vs_snip_loader = require('luasnip.loaders.from_vscode')
    vs_snip_loader.load({ paths = snip_paths })
    vs_snip_loader.load()

    snip_paths = {}

    local snip_loader = require('luasnip.loaders.from_lua')
    snip_loader.lazy_load({ paths = snip_paths })
end

return M
