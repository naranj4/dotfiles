--------------------------------------------------------------------------------
-- Finder Functions
--------------------------------------------------------------------------------
local M = {}

-- Use git_files until it doesn't work, then run find_files
function M.project_files(directory, skip_git)
    -- TODO: eventually let this take in a list of directories
    local opts = {}
    if directory ~= nil and directory ~= '' then
        opts.cwd = directory
    end

    -- first attempt to use git files
    if not skip_git then
        if pcall(require('telescope.builtin').git_files, opts) then return end
    end

    require('telescope.builtin').find_files(opts)
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
