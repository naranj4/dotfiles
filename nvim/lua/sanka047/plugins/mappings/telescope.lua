--------------------------------------------------------------------------------
-- Telescope Keymap
--------------------------------------------------------------------------------
local utils = require('sanka047.core.utils')
local map = utils.map
local map_group = utils.map_group

-- Use git_files until it doesn't work, then run find_files
utils.project_files = function()
    local ok = pcall(require('telescope.builtin').git_files)
    if not ok then require('telescope.builtin').find_files() end
end

utils.search_dotfiles = function ()
    require('telescope.builtin').find_files({
        prompt_title = '< Config >',
        cwd = '~/.config/nvim',
        hidden = true,
    })
end

utils.search_string = function(search_str, directory)
    local opts = { search = search_str, use_regex = true }
    if directory ~= nil and directory ~= '' then
        opts.cwd = directory
    end
    require('telescope.builtin').grep_string(opts)
end

vim.cmd([[
    command! -nargs=+ TelescopeRG lua require('sanka047.core.utils').search_string(<f-args>)
]])

map_group('n', '<leader>f', 'telescope-find')
map('n', '<leader>ff', 'Find Files', '<CMD>lua require("sanka047.core.utils").project_files()<CR>')
map('n', '<leader>fbb', 'Find Buffers', '<CMD>lua require("telescope.builtin").buffers()<CR>')
map('n', '<leader>fhs', 'Find History', '<CMD>lua require("telescope.builtin").oldfiles()<CR>')

map('n', '<leader>fbm', 'Find Bookmarks', '<CMD>lua require("telescope.builtin").marks()<CR>')
map('n', '<leader>fcs', 'Find Colorscheme', '<CMD>lua require("telescope.builtin").colorscheme()<CR>')

map('n', '<leader>fs', 'Find String', ':TelescopeRG<space>')
map('n', '<leader>fS', 'Find String (Cursor)', '<CMD>lua require("telescope.builtin").grep_string()<CR>')
map('n', '<leader>fl', 'Live Grep', '<CMD>lua require("telescope.builtin").live_grep()<CR>')

map('n', '<leader>fht', 'Find Help Tags', '<CMD>lua require("telescope.builtin").help_tags()<CR>')

map('n', '<leader>fd', 'Find Diagnostics', '<CMD>lua require("telescope.builtin").diagnostics()<CR>')

map('n', '<leader>fm', 'Find Treesitter', '<CMD>lua require("telescope.builtin").treesitter()<CR>')

map_group('n', '<leader>g', 'goto')
map('n', '<leader>gd', 'Get Def.', '<CMD>lua require("telescope.builtin").lsp_definitions()<CR>')
map('n', '<leader>gy', 'Get Type Def.', '<CMD>lua require("telescope.builtin").lsp_type_definitions()<CR>')
map('n', '<leader>gi', 'Get Impl.', '<CMD>lua require("telescope.builtin").lsp_implementations()<CR>')
map('n', '<leader>gr', 'Get Ref.', '<CMD>lua require("telescope.builtin").lsp_references()<CR>')

map('n', '<leader><leader>vf', 'Find Neovim Config File', '<CMD>lua require("sanka047.core.utils").search_dotfiles()<CR>')