--------------------------------------------------------------------------------
-- Logging Functions
--------------------------------------------------------------------------------
local M = {}

function M.log(log_level, msg, title)
    vim.notify(msg, log_level, { title = title })
end

function M.debug(msg, title)
    M.log(vim.log.levels.DEBUG, msg, title)
end

function M.info(msg, title)
    M.log(vim.log.levels.INFO, msg, title)
end

function M.warn(msg, title)
    M.log(vim.log.levels.WARN, msg, title)
end

function M.error(msg, title)
    M.log(vim.log.levels.ERROR, msg, title)
end

return M
