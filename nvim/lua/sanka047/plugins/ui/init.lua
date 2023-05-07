--------------------------------------------------------------------------------
-- Aesthetics and UI
--------------------------------------------------------------------------------
return {
    {
        'kyazdani42/nvim-web-devicons',
        lazy = true,
        config = function () LOAD_CONFIG('ui.devicons') end,
    },
    {
        'stevearc/dressing.nvim',
        config = function () LOAD_CONFIG('ui.dressing') end,
    },
    {
        'rcarriga/nvim-notify',
        config = function () LOAD_CONFIG('ui.notify') end,
    },
    {
        'folke/noice.nvim',
        event = 'VimEnter',
        enabled = false,
        dependencies = {'MunifTanjim/nui.nvim', 'rcarriga/nvim-notify'},
        config = function () LOAD_CONFIG('ui.noice') end,
    },
    {
        'j-hui/fidget.nvim',
        config = function () LOAD_CONFIG('ui.fidget') end,
    },

    {
        'RRethy/vim-illuminate',
        config = function () LOAD_CONFIG('ui.illuminate') end,
    },

    {
        'lukas-reineke/indent-blankline.nvim',
        config = function () LOAD_CONFIG('ui.indentline') end,
    },
    {
        'nvim-treesitter/nvim-treesitter-context',
        dependencies = {'nvim-treesitter/nvim-treesitter'},
        config = function () LOAD_CONFIG('ui.context') end,
    },
    {
        'nvim-lualine/lualine.nvim',
        dependencies = { {'kyazdani42/nvim-web-devicons'} },
        config = function () LOAD_CONFIG('ui.lualine') end,
    },
    {
        'b0o/incline.nvim',
        config = function () LOAD_CONFIG('ui.incline') end,
    },

    {
        'goolord/alpha-nvim',
        enabled = true,
        config = function () LOAD_CONFIG('ui.alpha') end,
    },

    {
        'stevearc/oil.nvim',
        config = function ()
            LOAD_CONFIG('ui.oil')
            LOAD_MAPPING('ui.oil')
        end
    },
    {
        'kevinhwang91/nvim-bqf', -- TODO: make this lazy
        dependencies = {
            { 'nvim-treesitter/nvim-treesitter' },
            { 'junegunn/fzf', build = function() vim.fn['fzf#install']() end },
        },
        config = function () LOAD_CONFIG('ui.bqf') end,
    },
    {
        'nacro90/numb.nvim',
        config = function () LOAD_CONFIG('ui.numb') end,
    },
    {
        'akinsho/toggleterm.nvim',
        config = function ()
            LOAD_CONFIG('ui.toggleterm')
            LOAD_MAPPING('ui.toggleterm')
        end,
    },
}
