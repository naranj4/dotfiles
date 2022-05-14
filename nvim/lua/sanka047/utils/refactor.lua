--------------------------------------------------------------------------------
-- Refactor Functions
--------------------------------------------------------------------------------
local M = {}

function M.select_refactor()
    require('telescope').extensions.refactoring.refactors()
end

function M.inline_variable()
    require('refactoring').refactor('Inline Variable')
end

return M
