--------------------------------------------------------------------------------
-- Fzf Extension
--------------------------------------------------------------------------------
local M = {}

function M.config()
    return {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = 'smart_case',
    }
end

return M
