--------------------------------------------------------------------------------
-- Query Linter Config
--------------------------------------------------------------------------------
local M = {}

function M.config()
    return {
        enable = true,
        use_virtual_text = true,
        lint_events = { 'BufWrite', 'CursorHold' },
    }
end

return M
