--------------------------------------------------------------------------------
-- Treesitter Keymap Documentation
--------------------------------------------------------------------------------
local wk = require('which-key')
local map = require('sanka047.core.utils').map

-- Incremental Selection
map('n', 'gnn', 'Treesitter Init Selection')
map('x', 'grn', 'Treesitter Node Inc.')
map('x', 'grm', 'Treesitter Node Dec.')
map('x', 'grc', 'Treesitter Scope Inc.')

-- Text Objects (Select)
map('v', 'af', 'outer function')
map('v', 'if', 'inner function')
map('v', 'ac', 'outer class')
map('v', 'ic', 'inner class')

-- Text Objects (Movement)
map('', ']m', 'Next Method Start')
map('', '[m', 'Prev Method Start')
map('', ']]', 'Next Class Start')
map('', '[[', 'Prev Class Start')
map('', ']M', 'Next Method End')
map('', '[M', 'Prev Method End')
map('', '][', 'Next Class End')
map('', '[]', 'Prev Class End')
