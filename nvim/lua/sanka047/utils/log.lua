--------------------------------------------------------------------------------
-- Logging Functions
--------------------------------------------------------------------------------
local M = {}

M.log_level = vim.log.levels.ERROR

function M.log(log_level, msg, title)
    vim.notify(msg, log_level, { title = title })
end

function M.debug(msg, title)
    if M.log_level > vim.log.levels.DEBUG then
        return
    end
    M.log(vim.log.levels.DEBUG, msg, title)
end

function M.info(msg, title)
    if M.log_level > vim.log.levels.INFO then
        return
    end
    M.log(vim.log.levels.INFO, msg, title)
end

function M.warn(msg, title)
    if M.log_level > vim.log.levels.WARN then
        return
    end
    M.log(vim.log.levels.WARN, msg, title)
end

function M.error(msg, title)
    if M.log_level > vim.log.levels.ERROR then
        return
    end
    M.log(vim.log.levels.ERROR, msg, title)
end

return M
