--------------------------------------------------------------------------------
-- Dashboard Config
--------------------------------------------------------------------------------
local M = {}

function M.setup()
    vim.g.dashboard_default_executive = 'telescope'

    vim.g.dashboard_custom_section = {
        find_file = {
            description = {' Find Files                             <L> f f'},
            command = 'lua require("sanka047.utils.finder").project_files()',
        },
        find_history = {
            description = {'ﭯ Recently Opened Files                <L> f h s'},
            command = 'lua require("telescope.builtin").oldfiles()',
        },
        last_session = {
            description = {' Open Last Session                  <L> <L> s l'},
            command = 'SessionLoad',
        },
        change_colorscheme = {
            description = {' Change Colorscheme                   <L> f c s'},
            command = 'lua require("telescope.builtin").colorscheme()',
        },
        find_word = {
            description = {' Find Word                              <L> f l'},
            command = 'lua require("telescope.builtin").live_grep()',
        },
        new_file = {
            description = {' Create New File                    <L> <L> n f'},
            command = 'DashboardNewFile',
        },
        packer_sync = {
            description = {' Sync Plugins                                  '},
            command = 'lua require("sanka047.utils.packer").packer_sync()',
        },
    }

    vim.g.dashboard_custom_header = {
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
end

return M
