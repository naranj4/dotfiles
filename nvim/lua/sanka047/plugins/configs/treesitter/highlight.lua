--------------------------------------------------------------------------------
-- Highlight Config
--------------------------------------------------------------------------------
local M = {}

function M.config()
    return {
        enable = true,
        custom_captures = {},
        additional_vim_regex_highlighting = false,
    }
end

return M
