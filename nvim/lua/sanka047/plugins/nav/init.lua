--------------------------------------------------------------------------------
-- Movement & File Navigation
--------------------------------------------------------------------------------
return {
    {
        'phaazon/hop.nvim',
        branch = 'v1',
        lazy = true,
        config = function () LOAD_CONFIG('nav.hop') end,
    },
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
        tag = '0.1.0',
        lazy = true,
        cmd = 'Telescope',
        dependencies = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-telescope/telescope-fzf-native.nvim', build = 'make'},
            {'nvim-telescope/telescope-hop.nvim'},
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
