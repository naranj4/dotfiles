--------------------------------------------------------------------------------
-- Telescope Config
--------------------------------------------------------------------------------
local actions = require('telescope.actions')
local telescope = require('telescope')
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
            find_command = {"fd", "--type", "f"},
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
    },
})
telescope.load_extension('fzf')

--------------------------------------------------------------------------------
-- Telescope Keymap
--------------------------------------------------------------------------------
local common = require('common')
local wk = require('which-key')

local exec = vim.api.nvim_exec

-- Use git_files until it doesn't work, then run find_files
common.project_files = function()
    local ok = pcall(require('telescope.builtin').git_files)
    if not ok then require('telescope.builtin').find_files() end
end

common.search_string = function(search_str)
    require('telescope.builtin').grep_string({ search = search_str, use_regex = true })
end
exec([[
    command! -nargs=1 TelescopeRG lua require('common').search_string(<f-args>)
]], false)

wk.register({
    ['<leader>f'] = {
        name = 'telescope-find',
        f = { '<CMD>lua require"common".project_files()<CR>', 'Find Files' },
        s = { ':TelescopeRG<space>', 'Find String' },
        S = { '<CMD>lua require"telescope.builtin".grep_string()<CR>', 'Find String (Cursor)' },
        l = { '<CMD>lua require"telescope.builtin".live_grep()<CR>', 'Live Grep' },
        b = { '<CMD>lua require"telescope.builtin".buffers()<CR>', 'Find Buffers' },
        h = { '<CMD>lua require"telescope.builtin".help_tags()<CR>', 'Find Help Tags' },
        d = { '<CMD>lua require"telescope.builtin".diagnostics()<CR>', 'Find Diagnostics' },
        m = { '<CMD>lua require"telescope.builtin".treesitter()<CR>', 'Find Diagnostics' },
    },
    g = {
        d = { '<CMD>lua require"telescope.builtin".lsp_definitions()<CR>', 'Get Def.' },
        y = { '<CMD>lua require"telescope.builtin".lsp_type_definitions()<CR>', 'Get Type Def.' },
        i = { '<CMD>lua require"telescope.builtin".lsp_implementations()<CR>', 'Get Impl.' },
        r = { '<CMD>lua require"telescope.builtin".lsp_references()<CR>', 'Get Ref.' },
    },
}, { mode = 'n' })
