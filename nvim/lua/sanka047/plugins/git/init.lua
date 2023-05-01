--------------------------------------------------------------------------------
-- Git
--------------------------------------------------------------------------------
return {
    {
        'lewis6991/gitsigns.nvim',
        dependencies = { 'nvim-lua/plenary.nvim' },
        config = function () LOAD_CONFIG('git.gitsigns') end,
    },
    {
        'tpope/vim-fugitive',
        config = function () LOAD_MAPPING('git.fugitive') end,
    },
    {
        'sindrets/diffview.nvim',
        dependencies = { 'nvim-lua/plenary.nvim' },
        cmd = { 'DiffviewOpen', 'DiffviewFileHistory' },
        config = function () LOAD_CONFIG('git.diffview') end,
    },
    {
        'TimUntersberger/neogit',
        lazy = true,
        dependencies = { {'sindrets/diffview.nvim'} },
        enabled = false, -- still not properly using this
        config = function () LOAD_CONFIG('git.neogit') end,
    },
}
