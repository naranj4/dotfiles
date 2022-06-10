--------------------------------------------------------------------------------
-- LuaSnip Keymap
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group

local M = {}

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
