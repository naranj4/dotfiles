--------------------------------------------------------------------------------
-- Mapping Functions
--------------------------------------------------------------------------------
local M = {}

-- Use git_files until it doesn't work, then run find_files
function M.project_files(directory)
    local opts = {}
    if directory ~= nil and directory ~= '' then
        opts.cwd = directory
    end
    local ok = pcall(require('telescope.builtin').git_files, opts)
    if not ok then require('telescope.builtin').find_files(opts) end
end

function M.search_dotfiles()
    require('telescope.builtin').find_files({
        prompt_title = '< Config >',
        cwd = '~/.config/nvim',
        hidden = true,
    })
end

function M.search_string(search_str, directory)
    local opts = { search = search_str, use_regex = true }
    if directory ~= nil and directory ~= '' then
        opts.cwd = directory
    end
    require('telescope.builtin').grep_string(opts)
end

function M.find_files_containing(args)
    local opts = {
        find_command = {
            "rg",
            "--ignore",
            "--hidden",
            "--color=never",
            "--files-with-matches",
            args,
        },
    }
    require('telescope.builtin').find_files(opts)
end

return M
