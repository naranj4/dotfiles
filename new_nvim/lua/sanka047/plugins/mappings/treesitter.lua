--------------------------------------------------------------------------------
-- Treesitter Keymap Documentation
--------------------------------------------------------------------------------
local map = require('sanka047.core.utils').map

-- Incremental Selection
map('n', 'gnn', 'Treesitter Init Selection')
map('x', 'grn', 'Treesitter Node Inc.')
map('x', 'grm', 'Treesitter Node Dec.')
map('x', 'grc', 'Treesitter Scope Inc.')

-- Text Objects (Select)
map('vo', 'af', 'outer function')
map('vo', 'if', 'inner function')
map('vo', 'ac', 'outer class')
map('vo', 'ic', 'inner class')

-- Text Objects (Movement)
map('', ']m', 'Next Method Start')
map('', '[m', 'Prev Method Start')
map('', ']]', 'Next Class Start')
map('', '[[', 'Prev Class Start')
map('', ']M', 'Next Method End')
map('', '[M', 'Prev Method End')
map('', '][', 'Next Class End')
map('', '[]', 'Prev Class End')
