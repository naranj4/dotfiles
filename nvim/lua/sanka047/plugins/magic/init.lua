--------------------------------------------------------------------------------
-- Editing Magic
--------------------------------------------------------------------------------
return {
    {
        'ojroques/nvim-osc52',
        config = function ()
            LOAD_CONFIG('magic.osc52')
            LOAD_MAPPING('magic.osc52')
        end,
    },
    {
        'windwp/nvim-autopairs',
        lazy = true,
        config = function () LOAD_CONFIG('magic.autopairs') end,
    },
    {
        'machakann/vim-sandwich',
        init = function ()
            vim.cmd([[
            let g:sandwich_no_default_key_mappings = 1
            let g:operator_sandwich_no_default_key_mappings = 1
            ]])
        end,
        config = function ()
            LOAD_CONFIG('magic.sandwich')
            LOAD_MAPPING('magic.sandwich')
        end,
    },
    {
        'junegunn/vim-easy-align',
        config = function () LOAD_MAPPING('magic.easy-align') end,
    },
    {
        'AckslD/nvim-trevJ.lua',
        config = function ()
            LOAD_CONFIG('magic.trevj')
            LOAD_MAPPING('magic.trevj')
        end,
    },
    {
        'danymat/neogen',
        lazy = true,
        dependencies = { { 'nvim-treesitter/nvim-treesitter' } },
        config = function () LOAD_CONFIG('magic.neogen') end,
    },
    {
        'numToStr/Comment.nvim',
        config = function () LOAD_CONFIG('magic.comment') end,
    },
    { 'tpope/vim-abolish' },
    {
        'gbprod/substitute.nvim',
        lazy = true,
        config = function () LOAD_CONFIG('magic.substitute') end,
    },
}
