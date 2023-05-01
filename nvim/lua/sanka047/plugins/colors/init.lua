--------------------------------------------------------------------------------
-- Colorschemes
--------------------------------------------------------------------------------
return {
    ----------------------------------------------------------------------------
    -- Color Visualization
    ----------------------------------------------------------------------------
    {
        'norcalli/nvim-colorizer.lua',
        cmd = 'ColorizerToggle',
        config = function () LOAD_CONFIG('colors.colorizer') end,
    },

    ----------------------------------------------------------------------------
    -- Colorschemes
    ----------------------------------------------------------------------------
    {
        'rebelot/kanagawa.nvim',
        priority = 1000,
        config = function () LOAD_CONFIG('colors.kanagawa') end,
    }, -- nice, nice highlighting, no real complaints off the top of my head, nice completion menu
    {
        'tomasiser/vim-code-dark',
        enabled = false,
        config = function () LOAD_CONFIG('colors.code-dark') end,
    }, -- pretty nice and visible
    { 'savq/melange', enabled = false },
}
