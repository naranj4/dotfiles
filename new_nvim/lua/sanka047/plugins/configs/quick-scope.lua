--------------------------------------------------------------------------------
-- Quickscope Config
--------------------------------------------------------------------------------
-- TODO: quickscope broken on start up
vim.g.qs_enable = 1
vim.g.qs_highlight_on_keys = { 'f', 'F', 't', 'T' }
vim.g.qs_lazy_highlight = 1
vim.g.qs_delay = 100
vim.g.qs_hi_priority = 2
vim.g.qs_max_chars = 200
vim.g.qs_buftype_blacklist = { 'terminal', 'nofile' }