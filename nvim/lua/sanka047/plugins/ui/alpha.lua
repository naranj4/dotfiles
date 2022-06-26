--------------------------------------------------------------------------------
-- Alpha Config
--------------------------------------------------------------------------------
local log = require('sanka047.utils.log')

local M = {}

function M.setup()
    local has_alpha, alpha = pcall(require, 'alpha')
    if not has_alpha then
        log.error('alpha not available', 'Config')
        return false
    end

    local dashboard = require('alpha.themes.dashboard')
    dashboard.section.header.val = {
        [[=================     ===============     ===============   ========  ========]],
        [[\\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //]],
        [[||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||]],
        [[|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||]],
        [[||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||]],
        [[|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||]],
        [[||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||]],
        [[|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||]],
        [[||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||]],
        [[||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||]],
        [[||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||]],
        [[||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||]],
        [[||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||]],
        [[||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||]],
        [[||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||]],
        [[||.=='    _-'                                                     `' |  /==.||]],
        [[=='    _-'                        N E O V I M                         \/   `==]],
        [[\   _-'                                                                `-_   /]],
        [[ `''                                                                      ``']],
    }
    dashboard.section.header.opts.hl = 'DashboardHeader'

    dashboard.section.buttons.val = {
        dashboard.button(
            '<L> f f', ' Find Files', '<CMD>lua require("sanka047.utils.finder").project_files()<CR>', {}
        ),
        dashboard.button(
            '<L> f l', ' Find Word', '<CMD>lua require("telescope.builtin").live_grep()<CR>', {}
        ),
        dashboard.button(
            '<L> f h s', 'ﭯ Recently Opened Files', '<CMD>lua require("telescope.builtin").oldfiles()<CR>', {}
        ),
        dashboard.button(
            '<L> <L> n f', ' Create New File', '<CMD>enew<CR>', {}
        ),
        dashboard.button(
            '<L> <L> p s', ' Sync Plugins', '<CMD>lua require("sanka047.utils.packer").packer_sync()<CR>', {}
        ),
    }
    dashboard.section.buttons.opts.hl = 'DashboardCenter'
    dashboard.section.buttons.opts.hl_shortcut = 'DashboardShortCut'

    local function footer()
        local plugins = #vim.tbl_keys(packer_plugins)
        local v = vim.version()
        local platform = vim.fn.has 'mac' == 1 and '' or (vim.fn.has 'win32' == 1 and '' or '')
        return string.format(' v%d.%d.%d    %s     Loaded %d plugins', v.major, v.minor, v.patch, platform, plugins)
    end
    dashboard.section.footer.val = footer()
    dashboard.section.footer.opts.hl = 'DashboardFooter'

    dashboard.config.opts.noautocmd = true

    alpha.setup(dashboard.config)
end

return M
