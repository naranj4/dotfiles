require('nebulous').setup({
    variant = "midnight",
    disable = {
        background = false,
        endOfBuffer = false,
        terminal_colors = false,
    },
    italic = {
        comments = false,
        keywords = true,
        functions = false,
        variables = true,
    },
    custom_colors = {}
})
