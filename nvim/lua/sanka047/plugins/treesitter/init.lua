--------------------------------------------------------------------------------
-- Treesitter
--------------------------------------------------------------------------------
return {
    {
        'nvim-treesitter/nvim-treesitter',
        build = ':TSUpdate',
        dependencies = {
            {'nvim-treesitter/nvim-treesitter-textobjects'},
            {'RRethy/nvim-treesitter-textsubjects'},
            {'JoosepAlviste/nvim-ts-context-commentstring'},
            {'RRethy/nvim-treesitter-endwise'},
            {'windwp/nvim-ts-autotag'},
            {'nvim-treesitter/playground', cmd = 'TSPlaygroundToggle'},
            {'yioneko/nvim-yati'},
        },
        config = function ()
            LOAD_CONFIG('treesitter.core')
            LOAD_MAPPING('treesitter.core')
        end,
    },
}
