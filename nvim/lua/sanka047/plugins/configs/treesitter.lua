--------------------------------------------------------------------------------
-- Treesitter Config
--------------------------------------------------------------------------------
local ok, treesitter_configs = pcall(require, 'nvim-treesitter.configs')
if not ok then
    vim.notify('nvim-treesitter not available', 'error')
    return false
end

local treesitter_install = require('nvim-treesitter.install')

treesitter_configs.setup({
    ensure_installed = {
        "json",
        "lua",
        "python",
        "ruby",
        "vim",
        "yaml",
        "query",
    },
    sync_install = false,
    ignore_install = {},

    highlight = {
        enable = true,
        custom_captures = {},
        additional_vim_regex_highlighting = false,
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
        },
    },

    textsubjects = {
        enable = true,
        prev_selection = '<M-,>',
        keymaps = {
            ['.'] = 'textsubjects-smart',
            ['<M-;>'] = 'textsubjects-container-outer',
            ['i<M-;>'] = 'textsubjects-container-inner',
        },
    },

    playground = {
        enable = true,
        disable = {},
        updatetime = 50,
        persist_queries = false,
        keybindings = {
            toggle_query_editor = 'o',
            toggle_hl_groups = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes = 'a',
            toggle_language_display = 'I',
            focus_language = 'f',
            unfocus_language = 'F',
            update = 'R',
            goto_node = '<cr>',
            show_help = '?',
        }
    },

    query_linter = {
        enable = true,
        use_virtual_text = true,
        lint_events = { 'BufWrite', 'CursorHold' },
    },

    context_commentstring = {
        enable = true,
    },

    autotag = {
        enable = true,
    },
})
treesitter_install.prefer_git = true
