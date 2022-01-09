--------------------------------------------------------------------------------
-- Treesitter Config
--------------------------------------------------------------------------------
require('nvim-treesitter.configs').setup({
    ensure_installed = {
        "json",
        "lua",
        "python",
        "ruby",
        "vim",
        "yaml",
    },
    sync_install = false,
    ignore_install = {},

    highlight = {
        enable = true,
        custom_captures = {},
        additional_vim_regex_highlighting = false,
    },

    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "gnn",
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm",
        }
    },

    indent = {
        enable = true,
    },

    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = '@class.inner',
            },
        },
        move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
                [']m'] = '@function.outer',
                [']]'] = '@class.outer',
            },
            goto_next_end = {
                [']M'] = '@function.outer',
                [']['] = '@class.outer',
            },
            goto_prev_start = {
                ['[m'] = '@function.outer',
                ['[['] = '@class.outer',
            },
            goto_prev_end = {
                ['[M'] = '@function.outer',
                ['[]'] = '@class.outer',
            },
        }
    },
})
require('nvim-treesitter.install').prefer_git = true
