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
})
require('nvim-treesitter.install').prefer_git = true
