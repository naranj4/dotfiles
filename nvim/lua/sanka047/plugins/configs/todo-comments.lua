--------------------------------------------------------------------------------
-- TodoComments Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local ok, todo_comments = pcall(require, 'todo-comments')
    if not ok then
        log.error('todo-comments not available', 'Config')
        return false
    end

    todo_comments.setup({})
end

return M
