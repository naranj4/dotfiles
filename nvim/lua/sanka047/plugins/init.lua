--------------------------------------------------------------------------------
-- Plugins
--------------------------------------------------------------------------------
local firstload = require('sanka047.plugins.firstload')

if firstload then
    return false
end

local packer = require('packer')

packer.init({
    display = {
        open_fn = function()
            return require('packer.util').float { border = 'rounded' }
        end,
        prompt_border = 'rounded',
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
})

--------------------------------------------------------------------------------
-- Keymaps
--------------------------------------------------------------------------------
local map = require('sanka047.utils.map').map

map('n', '<leader><leader>ps', 'Packer Sync', require('sanka047.utils.packer').packer_sync)

--------------------------------------------------------------------------------
-- Plugin List
--------------------------------------------------------------------------------
return require('packer').startup(function(use)
    use {
        'lewis6991/impatient.nvim',
        config = function () require('impatient').enable_profile() end,
    }
    use {'dstein64/vim-startuptime'}
    use {
        'antoinemadec/FixCursorHold.nvim',
        config = function () vim.g.cursorhold_updatetime = 100 end,
    }
    use {'nvim-lua/plenary.nvim'}

    -- let packer manager itself
    use {'wbthomason/packer.nvim'}

    -- Document Mapping
    use {
        'folke/which-key.nvim',
        config = function () LOAD_CONFIG('which-key') end,
    }

    -- Color Visualization
    use {
        'norcalli/nvim-colorizer.lua',
        cmd = 'ColorizerToggle',
        config = function () LOAD_CONFIG('colorizer') end,
    }

    -- Colorschemes
    use {
        'folke/tokyonight.nvim',
        config = function () LOAD_CONFIG('colors.tokyonight') end,
    } -- night is good
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
    } -- main, moon good, dawn okay, colorcolumn is bad
    use {
        'tomasiser/vim-code-dark',
        config = function () LOAD_CONFIG('colors.code-dark') end,
    } -- pretty nice and visible
    use {
        'yashguptaz/calvera-dark.nvim',
        config = function () LOAD_CONFIG('colors.calvera-dark') end,
    } -- pretty great, incsearch is the same as search highlighting tho, and not obvious visual selection
    use {
        'rebelot/kanagawa.nvim',
        config = function () LOAD_CONFIG('colors.kanagawa') end,
    } -- nice, nice highlighting, no real complaints off the top of my head, nice completion menu
    use {
        'navarasu/onedark.nvim',
        config = function () LOAD_CONFIG('colors.onedark') end,
    } -- fixes most of my problem with onedarkpro, pleasant to look at
    use {'savq/melange'}

    -- Aesthetics & UI
    use {
        'kyazdani42/nvim-web-devicons',
        config = function () LOAD_CONFIG('nvim-web-devicons') end,
    }
    use {
        'stevearc/dressing.nvim',
        config = function () LOAD_CONFIG('dressing') end,
    }
    use {
        'rcarriga/nvim-notify',
        config = function () LOAD_CONFIG('notify') end,
    }
    use {
        'j-hui/fidget.nvim',
        config = function () LOAD_CONFIG('fidget') end,
    }

    use {
        'sanka047/Shade.nvim',
        config = function ()
            LOAD_CONFIG('shade')
            LOAD_MAPPING('shade')
        end,
    }

    use {
        'edluffy/specs.nvim',
        event = 'VimEnter',
        config = function () LOAD_CONFIG('specs') end,
    }
    use {
        'RRethy/vim-illuminate',
        config = function () LOAD_CONFIG('illuminate') end,
    }

    use {
        'lukas-reineke/indent-blankline.nvim',
        event = 'BufRead',
        config = function () LOAD_CONFIG('indent-blankline') end,
    }
    use {
        'nvim-lualine/lualine.nvim',
        requires = { {'kyazdani42/nvim-web-devicons'} },
        after = 'nvim-web-devicons',
        config = function () LOAD_CONFIG('lualine') end,
    }
    use {
        'b0o/incline.nvim',
        after = 'nvim-web-devicons',
        config = function () LOAD_CONFIG('incline') end,
    }
    use {
        'SmiteshP/nvim-gps',
        after = 'nvim-web-devicons',
        config = function () LOAD_CONFIG('nvim-gps') end,
    }

    use {
        'glepnir/dashboard-nvim',
        requires = { {'telescope.nvim'} },
        config = function ()
            LOAD_CONFIG('dashboard')
            LOAD_MAPPING('dashboard')
        end,
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

    -- Git
    use {
        'lewis6991/gitsigns.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        event = 'BufRead',
        config = function () LOAD_CONFIG('gitsigns') end,
    }
    use {
        'tpope/vim-fugitive',
        config = function () LOAD_MAPPING('fugitive') end,
    }
    use {
        'sindrets/diffview.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function ()
            LOAD_CONFIG('diffview')
            LOAD_MAPPING('diffview')
        end,
    }
    use {
        'TimUntersberger/neogit',
        requires = { {'sindrets/diffview.nvim'} },
        module = 'neogit',
        config = function () LOAD_CONFIG('neogit') end,
    }

    -- Floating terminal (for ease of use with git)
    use {
        'akinsho/toggleterm.nvim',
        config = function ()
            LOAD_CONFIG('toggleterm')
            LOAD_MAPPING('toggleterm')
        end,
    }

    -- Editing Magic
    use {
        'windwp/nvim-autopairs',
        event = 'InsertEnter',
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
        event = 'BufRead',
        config = function ()
            LOAD_CONFIG('comment')
            LOAD_MAPPING('comment')
        end,
    }
    use {
        'ThePrimeagen/refactoring.nvim',
        module = 'sanka047.utils.refactor',
        requires = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-treesitter/nvim-treesitter'},
        },
        config = function () LOAD_CONFIG('refactoring') end,
    }

    -- Movement & File Navigation
    use {
        'nacro90/numb.nvim',
        event = 'CmdlineEnter',
        config = function () LOAD_CONFIG('numb') end,
    }
    use {
        'phaazon/hop.nvim',
        branch = 'v1',
        module = 'hop',
        config = function () LOAD_CONFIG('hop') end,
    }
    use {'unblevable/quick-scope'}
    use {
        'abecodes/tabout.nvim',
        after = {'nvim-cmp', 'nvim-treesitter'},
        requires = {'nvim-treesitter/nvim-treesitter'},
        config = function () LOAD_CONFIG('tabout') end,
    }

    use {
        'nvim-telescope/telescope.nvim',
        module = 'telescope',
        requires = {
            {'nvim-lua/plenary.nvim'},
            {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'},
            {'nvim-telescope/telescope-hop.nvim'},
            {'kyazdani42/nvim-web-devicons'},
        },
        config = function () LOAD_CONFIG('telescope') end,
    }

    -- Text Substitution/Processing
    use {'tpope/vim-abolish'}
    use {
        'gbprod/substitute.nvim',
        module = 'substitute',
        config = function () LOAD_CONFIG('substitute') end,
    }

    -- Text Parsing
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        event = 'VimEnter',
        requires = {
            {
                'nvim-treesitter/nvim-treesitter-textobjects',
                event = 'BufRead',
            },
            {
                'RRethy/nvim-treesitter-textsubjects',
                event = 'BufRead',
            },
            {
                'JoosepAlviste/nvim-ts-context-commentstring',
                event = 'BufRead',
            },
            {
                'RRethy/nvim-treesitter-endwise',
                event = 'InsertEnter',
            },
            {
                'windwp/nvim-ts-autotag',
                event = 'InsertEnter',
            },
            {
                'nvim-treesitter/playground',
                cmd = 'TSPlaygroundToggle',
            },
        },
        config = function ()
            LOAD_CONFIG('treesitter')
            LOAD_MAPPING('treesitter')
        end,
    }
    use {
        'romgrk/nvim-treesitter-context',
        event = 'BufRead',
        requires = {'nvim-treesitter/nvim-treesitter'},
        config = function () LOAD_CONFIG('treesitter-context') end,
    }

    -- Snippets
    use {
        'L3MON4D3/LuaSnip',
        module = 'luasnip',
        requires = {
            {'rafamadriz/friendly-snippets'}
        },
        config = function () LOAD_CONFIG('luasnip') end,
    }

    -- LSP and Autocompletion
    use {
        'williamboman/nvim-lsp-installer',
        after = 'nvim-lspconfig',
        requires = {
            {
                'neovim/nvim-lspconfig',
                event = 'BufRead',
            },
            {
                'ray-x/lsp_signature.nvim',
                after = 'nvim-lsp-installer',
                config = function () LOAD_CONFIG('lsp-signature') end,
            },
        },
        config = function ()
            LOAD_MAPPING('lsp-installer')
            LOAD_CONFIG('lsp-installer')
        end,
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

    -- Autocompletion (coq_nvim)
    -- TODO: To avoid the problems with mapping, drop the snippets from coq and instead use null-ls to
    -- inject luasnip as an LSP source. Then use Luasnip for flexible mappings and the rest of coq's
    -- defaults should work fine.
    -- use {
    --     'ms-jpq/coq_nvim',
    --     branch = 'coq', after = 'nvim-web-devicons',
    --     requires = {
    --         {'ms-jpq/coq.artifacts', branch = 'artifacts'},  -- snippets
    --         {'ms-jpq/coq.thirdparty', branch = '3p'},
    --     },
    -- }
end)
