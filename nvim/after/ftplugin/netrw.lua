local map = function (...) require('sanka047.utils.map').buf_map(0, ...) end

local mappings = require('sanka047.core.mappings')
local bidirectional_leap = require('sanka047.plugins.nav.leap').bidirectional_leap

-- return to original buffer
map('n', 'Q', 'quit-netrw', '<CMD>Rexplore<CR>')

-- window switching mappings to override defaults
mappings.win_move({ buffer = 0 })

-- use `<C-s>` for sorting and `s` for leap
map('nxo', '<C-s>', 'sort-by', ':call <SNR>57_NetrwSortStyle(1)<CR>')
map('nxo', 's', 'leap bidirectional', bidirectional_leap)
