--------------------------------------------------------------------------------
-- GitSigns Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, gs = pcall(require, 'gitsigns')
    if not ok then
        log.error('gitsigns not available', 'Config')
        return false
    end

    gs.setup({
        signs = {
            add = {hl = 'GitSignsAdd', text = '+', numhl='GitSignsAddNr', linehl='GitSignsAddLn'},
            change = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
            delete = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
            topdelete = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
            changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
        },
        signcolumn = true,  -- Toggle with `:Gitsigns toggle_signs`
        numhl = true,  -- Toggle with `:Gitsigns toggle_numhl`
        linehl = false,  -- Toggle with `:Gitsigns toggle_linehl`
        word_diff = false,  -- Toggle with `:Gitsigns toggle_word_diff`
        watch_gitdir = {
            interval = 1000,
            follow_files = true,
        },
        attach_to_untracked = true,
        current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
        current_line_blame_opts = {
            virt_text = true,
            virt_text_pos = 'right_align', -- 'eol' | 'overlay' | 'right_align'
            delay = 1000,
            ignore_whitespace = false,
        },
        current_line_blame_formatter_opts = {
            relative_time = false,
        },
        sign_priority = 6,
        update_debounce = 100,
        status_formatter = nil, -- Use default
        max_file_length = 40000,
        preview_config = {
            -- Options passed to nvim_open_win
            border = 'rounded',
            style = 'minimal',
            relative = 'cursor',
            row = 0,
            col = 1,
        },
        yadm = {
            enable = false,
        },
        on_attach = function (bufnr)
            local map = function (...) require('sanka047.utils.map').buf_map(bufnr, ...) end
            local map_group = function (...) require('sanka047.utils.map').buf_map_group(bufnr, ...) end

            -- Navigation
            map('', ']c', 'Next Hunk',
                function ()
                    if vim.wo.diff then return ']c' end
                    vim.schedule(gs.next_hunk)
                    return '<Ignore>'
                end,
                { expr = true }
            )
            map('', '[c', 'Prev Hunk',
                function ()
                    if vim.wo.diff then return '[c' end
                    vim.schedule(gs.prev_hunk)
                    return '<Ignore>'
                end,
                { expr = true }
            )

            -- Actions
            map_group('n', '<leader>h', 'git-signs-actions')
            map('nv', '<leader>hs', 'Stage Hunk', '<CMD>Gitsigns stage_hunk<CR>')
            map('nv', '<leader>hr', 'Reset Hunk', '<CMD>Gitsigns reset_hunk<CR>')
            map('n', '<leader>hu', 'Undo Stage Hunk', gs.undo_stage_hunk)
            map('n', '<leader>hS', 'Stage Buffer', gs.stage_buffer)
            map('n', '<leader>hR', 'Reset Buffer', gs.reset_buffer)
            map('n', '<leader>hp', 'Preview Hunk', gs.preview_hunk)
            map('n', '<leader>hb', 'Blame Line', function () gs.blame_line({ full = true }) end)
            map('n', '<leader>hd', 'Toggle Deleted', gs.toggle_deleted)

            -- Text Objects
            map('ox', 'ih', 'in hunk', ':<C-U>Gitsigns select_hunk<CR>')
        end,
    })
end

return M
