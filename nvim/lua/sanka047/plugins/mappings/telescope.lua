--------------------------------------------------------------------------------
-- Telescope Keymap
--------------------------------------------------------------------------------
local finder = require('sanka047.utils.finder')
local map = require('sanka047.utils.map').map
local map_group = require('sanka047.utils.map').map_group
local create_command = require('sanka047.utils.map').create_command

local M = {}

function M.keymap()
    create_command(
        'TRG',
        function (args) finder.search_string(unpack(args.fargs)) end,
        'Ripgrep for string in directory and pipe results to Telescope',
        { nargs = '+' }
    )
    create_command(
        'TFindFiles', -- just so that command completion makes directory suggestions
        function (args) finder.project_files(args.args) end,
        'Find files in a directory',
        { nargs = 1, complete = 'dir' }
    )
    create_command(
        'TFindContaining',
        function (args) finder.find_files_containing(args.args) end,
        'Find files containing string',
        { nargs = 1 }
    )

    map_group('n', '<leader>f', 'telescope-find')
    map('n', '<leader>ff', 'Find Files', finder.project_files)
    map('n', '<leader>fF', 'Find Files (in dir)', ':TFindFiles<space>')
    map('n', '<leader>fbb', 'Find Buffers', function () require('telescope.builtin').buffers() end)
    map('n', '<leader>fhs', 'Find History', function () require('telescope.builtin').oldfiles() end)

    map('n', '<leader>fbm', 'Find Bookmarks', function () require('telescope.builtin').marks() end)
    map('n', '<leader>fc', 'Find Containing', ':FindContaining<space>')

    map('n', '<leader>fs', 'Find String', ':TelescopeRG<space>')
    map('n', '<leader>fS', 'Find String (Cursor)', function () require('telescope.builtin').grep_string() end)
    map('n', '<leader>fl', 'Live Grep', function () require('telescope.builtin').live_grep() end)
    map('n', '<leader>/', 'Find in Buffer', function () require('telescope.builtin').current_buffer_fuzzy_find() end)

    map('n', '<leader>fhh', 'Find Help Tags', function () require('telescope.builtin').help_tags() end)

    map('n', '<leader>fm', 'Find Treesitter', function () require('telescope.builtin').treesitter() end)
    map('n', '<leader>fn', 'Find Notifications', function () require('telescope').extensions.notify.notify() end)
    map('n', '<leader>mf', 'Find Marks (Harpoon)', function () require('telescope').extensions.harpoon.marks() end)

    map('n', '<leader><leader>vf', 'Find Neovim Config File', finder.search_dotfiles)
end

return M
