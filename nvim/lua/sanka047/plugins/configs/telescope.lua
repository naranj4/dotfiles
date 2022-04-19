--------------------------------------------------------------------------------
-- Telescope Config
--------------------------------------------------------------------------------
local ok, telescope = pcall(require, 'telescope')
if not ok then
    vim.notify('telescope not available', 'error')
    return false
end

local actions = require('telescope.actions')
telescope.setup({
    defaults = {
        -- Default configuration for telescope goes here:
        -- config_key = value,
        vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
            "--trim",
        },
        mappings = {
            i = {
                -- map actions.which_key to <C-h> (default: <C-/>)
                -- actions.which_key shows the mappings for your picker,
                -- e.g. git_{create, delete, ...}_branch for the git_branches picker
                ["<C-h>"] = actions.which_key,
                ["<ESC>"] = actions.close,
                ["<C-Space>"] = function(prompt_bufnr)
                    local opts = {
                        callback = actions.toggle_selection,
                        loop_callback = actions.send_selected_to_qflist,
                    }
                    require('telescope').extensions.hop._hop_loop(prompt_bufnr, opts)
                end,
            },
        },
    },
    pickers = {
        -- Default configuration for builtin pickers goes here:
        -- picker_name = {
        --   picker_config_key = value,
        --   ...
        -- }
        -- Now the picker_config_key will be applied every time you call this
        -- builtin picker
        find_files = {
            -- find_command = {"fd", "--type", "f"},
            find_command = {"rg", "--ignore", "--hidden", "--files"},
        },
        git_files = {},
        diagnostics = {
            bufnr = 0,
        },
    },
    extensions = {
        -- Your extension configuration goes here:
        -- extension_name = {
        --   extension_config_key = value,
        -- }
        -- please take a look at the readme of the extension you want to configure
        fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = 'smart_case',
        },
        hop = {
            keys = {
                "a", "s", "d", "f", "g", "h", "j", "k", "l", ";",
                "q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
                "A", "S", "D", "F", "G", "H", "J", "K", "L", ":",
                "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P",
            },
            sign_hl = { 'Title', 'Title' },
            line_hl = { 'Normal', 'Normal' },
            clear_selection_hl = false,
            trace_entry = true,
            reset_selection = true,
        },
    },
})
telescope.load_extension('fzf')
telescope.load_extension('hop')
telescope.load_extension('notify')
