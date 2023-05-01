--------------------------------------------------------------------------------
-- LSP
--------------------------------------------------------------------------------
return {
    {
        'williamboman/mason-lspconfig.nvim',
        dependencies = { {'williamboman/mason.nvim'} },
        config = function () LOAD_CONFIG('lsp.mason') end,
    },
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            {'williamboman/mason.nvim'},
            {'williamboman/mason-lspconfig.nvim'},
        },
        config = function ()
            LOAD_CONFIG('lsp.config')
            LOAD_MAPPING('lsp.config')
        end,
    },
    {
        'ray-x/lsp_signature.nvim',
        config = function () LOAD_CONFIG('lsp.signature') end,
    },

    ----------------------------------------------------------------------------
    -- Autocompletion (nvim-cmp)
    ----------------------------------------------------------------------------
    {
        'hrsh7th/nvim-cmp',
        lazy = true,
        dependencies = {
            { 'onsails/lspkind.nvim' },
            { 'hrsh7th/cmp-nvim-lsp' },
            { 'hrsh7th/cmp-buffer' },
            { 'hrsh7th/cmp-path' },
            { 'hrsh7th/cmp-cmdline' },
            { 'saadparwaiz1/cmp_luasnip', dependencies = { 'L3MON4D3/LuaSnip' } },
        },
        config = function ()
            LOAD_CONFIG('lsp.cmp')
            LOAD_MAPPING('lsp.cmp')
        end,
    },

    ----------------------------------------------------------------------------
    -- Snippets
    ----------------------------------------------------------------------------
    {
        'L3MON4D3/LuaSnip',
        lazy = true,
        dependencies = {
            { 'rafamadriz/friendly-snippets' },
        },
        config = function () LOAD_CONFIG('lsp.luasnip') end,
    },
}
