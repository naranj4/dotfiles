--------------------------------------------------------------------------------
-- Neovim Config File
--------------------------------------------------------------------------------
-- Design:
-- - Plugin mapping will be available prior to the plugin has initialized
-- - Plugins will load their configs when they are loaded by packer
-- - Packer will be downloaded on first load
require('sanka047.core.globals')
require('sanka047.core.options')
require('sanka047.core.plugins')
require('sanka047.core.colors')
require('sanka047.core.mappings')
