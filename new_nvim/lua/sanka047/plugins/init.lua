--------------------------------------------------------------------------------
-- Plugin List
--------------------------------------------------------------------------------
local firstload = require('sanka047.plugins.firstload')

if firstload then
    return false
end

local packer = require('packer')

packer.init {
    display = {
        open_fn = function()
            return require("packer.util").float { border = "double" }
        end,
        prompt_border = "single",
    },
    git = {
        clone_timeout = 300, -- seconds
    },
    auto_clean = true,
    compile_on_sync = true,
    profile = {
        enable = true,
        threshold = 1,
    },
}

return require('packer').startup(function(use)
    use {'nvim-lua/plenary.nvim'}

    -- let packer manager itself
    use {'wbthomason/packer.nvim'}

    -- Document Mapping
    use {
        'folke/which-key.nvim',
        config = function () LOAD_CONFIG('which-key') end,
    }

    -- Colorschemes
    use {
        'folke/tokyonight.nvim',
        config = function () LOAD_CONFIG('colors.tokyonight') end,
    } -- night is good
    use {
        'whatyouhide/vim-gotham',
        config = function () LOAD_CONFIG('colors.gotham') end,
    } -- not treesitter compatible, but awesome
    use {
        'EdenEast/nightfox.nvim',
        config = function () LOAD_CONFIG('colors.nightfox') end,
    } -- tbf, nightfox isn't that bad, nordfox meh, dayfox alright, duskfox is nice
    use {
        'marko-cerovac/material.nvim',
        config = function () LOAD_CONFIG('colors.material') end,
    }
    use {
        'rose-pine/neovim',
        as = 'rose-pine',
        config = function () LOAD_CONFIG('colors.rose-pine') end,
    } -- main, moon good, dawn okay
    use {
        'tomasiser/vim-code-dark',
        config = function () LOAD_CONFIG('colors.code-dark') end,
    } -- pretty nice and visible
    use {
        'sainnhe/edge',
        config = function () LOAD_CONFIG('colors.edge') end,
    } -- light theme is nice

    -- Aesthetics & UI
    use {
        'kyazdani42/nvim-web-devicons',
        config = function () LOAD_CONFIG('nvim-web-devicons') end,
    }

    -- use {'sunjon/Shade.nvim'}  -- currently there is a bug with floating windows being unable to close
    -- use {'karb94/neoscroll.nvim'}  -- a bit slow for my liking

    use {
        'nvim-lualine/lualine.nvim',
        requires = { {'kyazdani42/nvim-web-devicons'} },
        after = 'nvim-web-devicons',
        config = function () LOAD_CONFIG('lualine') end,
    }
    use {
        'kyazdani42/nvim-tree.lua',
        requires = { {'kyazdani42/nvim-web-devicons'} },
        cmd = {'NvimTreeToggle', 'NvimTreeFindFileToggle'},
        config = function ()
            LOAD_CONFIG('nvim-tree')
            LOAD_MAPPING('nvim-tree')
        end,
    }
    use {
        'lewis6991/gitsigns.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        event = {'BufRead', 'BufEnter'},
        config = function ()
            LOAD_CONFIG('gitsigns')
            LOAD_MAPPING('gitsigns')
        end,
    }

    -- Parentheses & Comment Magic
    use {
        'windwp/nvim-autopairs',
        config = function ()
            LOAD_CONFIG('nvim-autopairs')
            LOAD_MAPPING('nvim-autopairs')
        end,
    }
    use {
        'machakann/vim-sandwich',
        config = function ()
            LOAD_CONFIG('sandwich')
            LOAD_MAPPING('sandwich')
        end,
    }

    use {
        'numToStr/Comment.nvim',
        config = function ()
            LOAD_CONFIG('comment')
            LOAD_MAPPING('comment')
        end,
    }

    -- Movement & File Navigation
    use {
        'phaazon/hop.nvim',
        branch = 'v1',
        event = 'VimEnter',
        config = function () LOAD_CONFIG('hop') end,
    }
    use {'unblevable/quick-scope'}
    -- use {'ggandor/lightspeed.nvim', after = 'nvim-web-devicons'}

    use {
        'nvim-telescope/telescope.nvim',
        event = 'VimEnter',
        requires = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'},
            {'kyazdani42/nvim-web-devicons'},
        },
        config = function () LOAD_CONFIG('telescope') end,
    }

    -- Text Parsing
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        event = 'BufRead',
        requires = {
            {'nvim-treesitter/nvim-treesitter-textobjects', after = 'nvim-treesitter'},
        },
        config = function ()
            LOAD_CONFIG('treesitter')
            LOAD_MAPPING('treesitter')
        end,
    }

    -- Snippets
    use {
        'L3MON4D3/LuaSnip',
        event = 'BufRead',
        requires = {
            {'rafamadriz/friendly-snippets'}
        },
        config = function () LOAD_CONFIG('luasnip') end,
    }

    -- LSP and Autocompletion
    use {
        'neovim/nvim-lspconfig',
        event = 'BufRead',
        config = function () LOAD_CONFIG('lspconfig') end,
    }

    -- Autocompletion (nvim-cmp)
    use {
        'hrsh7th/nvim-cmp',
        event = 'InsertEnter',
        requires = {
            {'hrsh7th/cmp-nvim-lsp', after = 'nvim-cmp'},
            {'hrsh7th/cmp-buffer', after = 'nvim-cmp'},
            {'hrsh7th/cmp-path', after = 'nvim-cmp'},
            {'hrsh7th/cmp-cmdline', after = 'nvim-cmp'},
            {'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp'},
            {'saadparwaiz1/cmp_luasnip', after = {'nvim-cmp', 'LuaSnip'}},
        },
        config = function ()
            LOAD_CONFIG('nvim-cmp')
            LOAD_MAPPING('nvim-cmp')
        end,
    }

    -- -- Autocompletion (coq_nvim)
    -- use {
    --     'ms-jpq/coq_nvim',
    --     branch = 'coq', after = 'nvim-web-devicons',
    --     requires = {
    --         {'ms-jpq/coq.artifacts', branch = 'artifacts'},  -- snippets
    --         {'ms-jpq/coq.thirdparty', branch = '3p'},
    --     },
    -- }
end)
