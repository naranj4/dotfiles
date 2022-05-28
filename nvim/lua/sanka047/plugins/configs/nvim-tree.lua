--------------------------------------------------------------------------------
-- Nvim-Tree Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, nvim_tree = pcall(require, 'nvim-tree')
    if not ok then
        log.error('nvim-tree not available', 'Config')
        return false
    end

    nvim_tree.setup({
        disable_netrw = true,
        hijack_netrw = true,
        open_on_setup = false,
        ignore_ft_on_setup = {},
        open_on_tab = false,
        hijack_cursor = true,
        update_cwd = true,
        update_to_buf_dir = {
            enable = true,
            auto_open = true,
        },
        diagnostics = {
            enable = true,
            icons = {
                hint = "",
                info = "",
                warning = "",
                error = "",
            }
        },
        update_focused_file = {
            enable = false,
            update_cwd = false,
            ignore_list = {}
        },
        system_open = {
            cmd = nil,
            args = {}
        },
        filters = {
            dotfiles = false,
            custom = {}
        },
        git = {
            enable = true,
            ignore = true,
            timeout = 500,
        },
        view = {
            width = 50,
            height = 30,
            hide_root_folder = false,
            side = 'left',
            auto_resize = false,
            mappings = {
                custom_only = false,
                list = {
                    { key = '?', action = 'toggle_help' },
                },
            },
            number = false,
            relativenumber = false,
            signcolumn = 'yes',
        },
        renderer = {
            indent_markers = {
                enable = true,
            },
            highlight_git = true,
            highlight_opened_files = 'name',
            group_empty = true,
            add_trailing = true,
        },
        actions = {
            open_file = {
                quit_on_open = true,
                window_picker = {
                    enable = true,
                    exclude = {
                        filetype = {
                            'notify',
                            'packer',
                            'qf'
                        },
                        buftype = {
                            'terminal'
                        },
                    },
                },
            },
        },
        trash = {
            cmd = "trash",
            require_confirm = true
        }
    })

    -- Auto-closing
    vim.cmd([[autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif]])
end

return M
