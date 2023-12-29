--------------------------------------------------------------------------------
-- Quickscope Config
--------------------------------------------------------------------------------
local M = {}

function M.setup()
    vim.g.qs_delay = 100
    vim.g.qs_hi_priority = 2
    vim.g.qs_max_chars = 200
end

return M
