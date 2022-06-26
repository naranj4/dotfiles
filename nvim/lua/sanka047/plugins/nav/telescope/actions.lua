--------------------------------------------------------------------------------
-- Telescope Actions
--------------------------------------------------------------------------------
local actions = require('telescope.actions')

local M = {}

function M.open_in_qflist(prompt_bufnr)
    actions.smart_send_to_qflist(prompt_bufnr)
    actions.open_qflist(prompt_bufnr)
end

return M
