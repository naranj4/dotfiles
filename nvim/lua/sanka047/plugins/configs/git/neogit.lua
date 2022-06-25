--------------------------------------------------------------------------------
-- Neogit Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local ok, neogit = pcall(require, 'neogit')
    if not ok then
        log.error('neogit not available', 'Config')
        return false
    end

    neogit.setup {
        disable_signs = false,
        disable_hint = false,
        disable_context_highlighting = false,
        disable_commit_confirmation = true,
        -- Neogit refreshes its internal state after specific events, which can be expensive depending on the repository size. 
        -- Disabling `auto_refresh` will make it so you have to manually refresh the status after you open it.
        auto_refresh = true,
        disable_builtin_notifications = false,
        use_magit_keybindings = false,
        commit_popup = { kind = 'floating' },
        -- Change the default way of opening neogit
        kind = 'tab',
        -- customize displayed signs
        signs = {
            -- { CLOSED, OPENED }
            section = { '', '' },
            item = { '', '' },
            hunk = { '', '' },
        },
        integrations = {
            -- Neogit only provides inline diffs. If you want a more traditional way to look at diffs, you can use `sindrets/diffview.nvim`.
            -- The diffview integration enables the diff popup, which is a wrapper around `sindrets/diffview.nvim`.
            --
            -- Requires you to have `sindrets/diffview.nvim` installed.
            -- use {
            --   'TimUntersberger/neogit',
            --   requires = {
            --     'nvim-lua/plenary.nvim',
            --     'sindrets/diffview.nvim'
            --   }
            -- }
            --
            diffview = true
        },
        -- Setting any section to `false` will make the section not render at all
        sections = {
            untracked = { folded = true },
            unstaged = { folded = false },
            staged = { folded = false },
            stashes = { folded = true },
            unpulled = { folded = true },
            unmerged = { folded = false },
            recent = { folded = true },
        },
        -- override/add mappings
        mappings = {
            -- modify status buffer mappings
            status = {
                -- Defaults
                ['q'] = 'Close',

                ['1'] = 'Depth1',
                ['2'] = 'Depth2',
                ['3'] = 'Depth3',
                ['4'] = 'Depth4',

                ['<Tab>'] = 'Toggle',
                ['<CR>'] = 'GoToFile',

                ['x'] = 'Discard',

                ['s'] = 'Stage',
                ['S'] = 'StageUnstaged',
                ['<C-s>'] = 'StageAll',

                ['u'] = 'Unstage',
                ['U'] = 'UnstageUnstaged',

                ['<C-r>'] = 'RefreshBuffer',
                ['$'] = 'CommandHistory',

                ['c'] = 'CommitPopup',
                ['p'] = 'PullPopup',
                ['P'] = 'PushPopup',
                ['r'] = 'RebasePopup',
                ['L'] = 'LogPopup',
                ['b'] = 'BranchPopup',
                ['Z'] = 'StashPopup',

                ['?'] = 'HelpPopup',
            }
        }
    }
end

--------------------------------------------------------------------------------
-- Neogit Keymap
--------------------------------------------------------------------------------
function M.keymap()
    map('n', '<leader>ng', 'Open Neogit', function () require('neogit').open() end)
end

return M
