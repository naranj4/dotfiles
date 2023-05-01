--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------
return {
    {
        'williamboman/mason.nvim',
        config = function () LOAD_CONFIG('misc.mason') end,
    },
    {
        'folke/which-key.nvim',
        config = function () LOAD_CONFIG('misc.which-key') end,
    },
}
