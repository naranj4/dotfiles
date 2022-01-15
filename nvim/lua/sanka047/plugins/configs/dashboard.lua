--------------------------------------------------------------------------------
-- Dashboard Config
--------------------------------------------------------------------------------
vim.g.dashboard_default_executive = 'telescope'

vim.g.dashboard_custom_section = {
    find_file = {
        description = {' Find Files                             <L> f f'},
        command = 'lua require("sanka047.core.utils").project_files()',
    },
    find_history = {
        description = {'ﭯ Recently Opened Files                <L> f h s'},
        command = 'Telescope oldfiles',
    },
    last_session = {
        description = {' Open Last Session                  <L> <L> s l'},
        command = 'SessionLoad',
    },
    change_colorscheme = {
        description = {' Change Colorscheme                   <L> f c s'},
        command = 'Telescope colorscheme',
    },
    find_word = {
        description = {' Find Word                              <L> f l'},
        command = 'Telescope live_grep',
    },
    new_file = {
        description = {' Create New File                    <L> <L> n f'},
        command = 'DashboardNewFile',
    },
    packer_sync = {
        description = {' Sync Plugins                                  '},
        command = 'PackerSync',
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
