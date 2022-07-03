--------------------------------------------------------------------------------
-- Neovim Config File
--------------------------------------------------------------------------------
-- impatient for loading performance
local has_impatient, impatient = pcall(require, 'impatient')
if has_impatient then
    impatient.enable_profile()
end

-- Design:
-- - Plugin mapping will be available prior to the plugin has initialized
-- - Plugins will load their configs when they are loaded by packer
-- - Packer will be downloaded on first load
require('sanka047.core.globals')
require('sanka047.core.options')
require('sanka047.core.project')
require('sanka047.core.plugins')
require('sanka047.core.colors')
require('sanka047.core.mappings')
