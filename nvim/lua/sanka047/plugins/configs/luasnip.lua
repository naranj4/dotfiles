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

    local types = require('luasnip.util.types')

    luasnip.config.setup({
        history = true,
        update_events = 'TextChanged,TextChangedI',
        ext_opts = {
            [types.choiceNode] = {
                active = {
                    virt_text = { { '●', { 'Boolean', 'CursorLine' } } },
                },
            },
        },
    })

    -- add snippet folders here
    local snip_paths = { '~/.config/nvim/lua/sanka047/snippets/' }

    local snip_loader = require('luasnip.loaders.from_lua')
    snip_loader.lazy_load({ paths = snip_paths })

    snip_paths = {}

    local vs_snip_loader = require('luasnip.loaders.from_vscode')
    vs_snip_loader.lazy_load({ paths = snip_paths })
    vs_snip_loader.lazy_load()
end

return M
