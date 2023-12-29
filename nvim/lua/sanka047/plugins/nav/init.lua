--------------------------------------------------------------------------------
-- Movement & File Navigation
--------------------------------------------------------------------------------
return {
    { 'unblevable/quick-scope' },
    {
        'ggandor/leap.nvim',
        config = function ()
            LOAD_CONFIG('nav.leap')
            LOAD_MAPPING('nav.leap')
        end
    },

    {
        'nvim-telescope/telescope.nvim',
        branch = '0.1.x',
        lazy = true,
        cmd = 'Telescope',
        dependencies = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-telescope/telescope-fzf-native.nvim', build = 'make'},
            {'kyazdani42/nvim-web-devicons'},
        },
        config = function () LOAD_CONFIG('nav.telescope') end,
    },
    {
        'ThePrimeagen/harpoon',
        dependencies = { {'nvim-lua/plenary.nvim'} },
        config = function ()
            LOAD_CONFIG('nav.harpoon')
            LOAD_MAPPING('nav.harpoon')
        end,
    },
}
