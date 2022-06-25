--------------------------------------------------------------------------------
-- LuaSnip Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

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

--------------------------------------------------------------------------------
-- LuaSnip Keymap
--------------------------------------------------------------------------------
function M.keymap()
    map_group('n', '<leader><leader>s', 'snippets')
    map('n', '<leader><leader>sr', 'Reload Luasnip', function ()
        R('sanka047.plugins.configs.luasnip').setup()
    end)
    map('n', '<leader><leader>se', 'Edit Snippets', function ()
        require('luasnip.loaders').edit_snippet_files({})
    end)
    map('n', '<leader><leader>su', 'Unlink Current Snippet', function ()
        require('luasnip').unlink_current()
    end)

    -- insert mode snippet movement
    map('is', '<C-o>', 'expand snippet', function () require('luasnip').expand() end)
    map('is', '<C-j>', 'jump forward', function () require('luasnip').jump(1) end)
    map('is', '<C-k>', 'jump backward', function () require('luasnip').jump(-1) end)

    map('is', '<C-n>', 'cycle forward', function ()
        local ls = require('luasnip')
        if ls.choice_active() then
            ls.change_choice(1)
        end
    end)
    map('is', '<C-p>', 'cycle backward', function ()
        local ls = require('luasnip')
        if ls.choice_active() then
            ls.change_choice(-1)
        end
    end)
end

return M
