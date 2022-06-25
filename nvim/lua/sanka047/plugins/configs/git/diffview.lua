--------------------------------------------------------------------------------
-- Diffview Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')
local map = require('sanka047.utils.map').map

local M = {}

function M.setup()
    local ok, diffview = pcall(require, 'diffview')
    if not ok then
        log.error('diffview not available', 'Config')
        return false
    end

    local actions = require('diffview.actions')

    diffview.setup({
        diff_binaries = false,    -- Show diffs for binaries
        enhanced_diff_hl = true,  -- See ':h diffview-config-enhanced_diff_hl'
        use_icons = true,         -- Requires nvim-web-devicons
        icons = {                 -- Only applies when use_icons is true.
            folder_closed = '',
            folder_open = '',
        },
        signs = {
            fold_closed = '',
            fold_open = '',
        },
        file_panel = {
            win_config = {
                position = 'left',                  -- One of 'left', 'right', 'top', 'bottom'
                width = 40,                         -- Only applies when position is 'left' or 'right'
                height = 10,                        -- Only applies when position is 'top' or 'bottom'
            },
            listing_style = 'tree',             -- One of 'list' or 'tree'
            tree_options = {                    -- Only applies when listing_style is 'tree'
                flatten_dirs = true,              -- Flatten dirs that only contain one single dir
                folder_statuses = 'only_folded',  -- One of 'never', 'only_folded' or 'always'.
            },
        },
        file_history_panel = {
            win_config = {
                position = 'bottom',
                width = 35,
                height = 16,
            },
            log_options = {
                single_file = {
                    max_count = 512,      -- Limit the number of commits
                    follow = true,       -- Follow renames (only for single file)
                    all = false,          -- Include all refs under 'refs/' including HEAD
                    merges = false,       -- List only merge commits
                    no_merges = false,    -- List no merge commits
                    reverse = false,      -- List commits in reverse order
                },
                multi_file = {
                    max_count = 256,      -- Limit the number of commits
                    all = false,          -- Include all refs under 'refs/' including HEAD
                    merges = false,       -- List only merge commits
                    no_merges = false,    -- List no merge commits
                    reverse = false,      -- List commits in reverse order
                },
            },
        },
        default_args = {    -- Default args prepended to the arg-list for the listed commands
            DiffviewOpen = { '--untracked-files' },
            DiffviewFileHistory = {},
        },
        hooks = {},         -- See ':h diffview-config-hooks'

        key_bindings = {
            disable_defaults = true,                   -- Disable the default key bindings
            -- The `view` bindings are active in the diff buffers, only when the current
            -- tabpage is a Diffview.
            view = {
                ['<M-j>']      = actions.select_next_entry,  -- Open the diff for the next file
                ['<M-k>']      = actions.select_prev_entry,  -- Open the diff for the previous file
                ['<leader>f']  = actions.focus_entry,
                ['gf']         = actions.goto_file_edit,     -- Open the file in a new split in previous tabpage
                ['<C-w><C-f>'] = actions.goto_file_split,    -- Open the file in a new split
                ['<C-w>gf']    = actions.goto_file_tab,      -- Open the file in a new tabpage
                ['<leader>n']  = actions.toggle_files,       -- Toggle the files panel.
                ['q']          = actions.close,
            },

            file_panel = {
                ['j']             = actions.next_entry,           -- Bring the cursor to the next file entry
                ['k']             = actions.prev_entry,           -- Bring the cursor to the previous file entry.
                ['<cr>']          = actions.select_entry,         -- Open the diff for the selected entry.
                ['o']             = actions.select_entry,
                ['s']             = actions.toggle_stage_entry,   -- Stage / unstage the selected entry.
                ['S']             = actions.stage_all,            -- Stage all entries.
                ['U']             = actions.unstage_all,          -- Unstage all entries.
                ['X']             = actions.restore_entry,        -- Restore entry to the state on the left side.
                ['R']             = actions.refresh_files,        -- Update stats and entries in the file list.
                ['<M-j>']         = actions.select_next_entry,    -- Open the diff for the next file
                ['<M-k>']         = actions.select_prev_entry,    -- Open the diff for the previous file
                ['gf']            = actions.goto_file_edit,
                ['<C-w><C-f>']    = actions.goto_file_split,
                ['<C-w>gf']       = actions.goto_file_tab,
                ['i']             = actions.listing_style,        -- Toggle between 'list' and 'tree' views
                ['f']             = actions.toggle_flatten_dirs,  -- Flatten empty subdirectories in tree listing style.
                ['<leader>n']     = actions.toggle_files,
                ['q']             = actions.close,
            },

            file_history_panel = {
                ['g!']            = actions.options,            -- Open the option panel
                ['<C-A-d>']       = actions.open_in_diffview,   -- Open the entry under the cursor in a diffview
                ['y']             = actions.copy_hash,          -- Copy the commit hash of the entry under the cursor
                ['zR']            = actions.open_all_folds,
                ['zM']            = actions.close_all_folds,
                ['j']             = actions.next_entry,
                ['k']             = actions.prev_entry,
                ['<cr>']          = actions.select_entry,
                ['o']             = actions.select_entry,
                ['<M-j>']         = actions.select_next_entry,
                ['<M-k>']         = actions.select_prev_entry,
                ['gf']            = actions.goto_file,
                ['<C-w><C-f>']    = actions.goto_file_split,
                ['<C-w>gf']       = actions.goto_file_tab,
                ['<leader>n']     = actions.toggle_files,
                ['q']             = actions.close,
            },

            option_panel = {
                ['q']     = actions.close,
            },
        },
    })
end

--------------------------------------------------------------------------------
-- Diffview Keymap
--------------------------------------------------------------------------------
function M.keymap()
    map('n', '<leader><leader>gd', 'Open Diffview', ':DiffviewOpen<space>')
    map('n', '<leader><leader>gh', 'Open file history', ':DiffviewFileHistory<space>')
end

return M
