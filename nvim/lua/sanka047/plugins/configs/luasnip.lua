--------------------------------------------------------------------------------
-- LuaSnip Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local ok, luasnip = pcall(require, 'luasnip')
if not ok then
    log.error('luasnip not available', 'Config')
    return false
end

-- add snippet folders here
local snip_paths = {}

luasnip.config.setup({
    history = true,
})

local snip_loader = require('luasnip.loaders.from_vscode')

snip_loader.load({ paths = snip_paths })
snip_loader.load()
