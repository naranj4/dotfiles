--------------------------------------------------------------------------------
-- Lightspeed Config
--------------------------------------------------------------------------------
require('lightspeed').setup({
    ignore_case = false,
    exit_after_idle_msecs = { unlabeled = 1000, labeled = nil },
    grey_out_search_area = true,
    highlight_unique_chars = true,
    jump_on_partial_input_safety_timeout = 400,
    match_only_the_start_of_same_char_seqs = true,

    safe_labels = {
        's', 'f', 'n',
        'u', 't',
        '/', 'F', 'L', 'N', 'H', 'G', 'M', 'U', 'T', '?', 'Z'
    },
    labels = {
        's', 'f', 'n',
        'j', 'k', 'l', 'o', 'i', 'w', 'e', 'h', 'g',
        'u', 't',
        'm', 'v', 'c', 'a', '.', 'z',
        '/', 'F', 'L', 'N', 'H', 'G', 'M', 'U', 'T', '?', 'Z'
    },
    cycle_group_fwd_key = '<space>',
    cycle_group_bwd_key = '<tab>',
})
