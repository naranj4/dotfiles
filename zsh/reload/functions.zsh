################################################################################
# ZSH Functions
################################################################################

# QoL reload global config
function greload {
    source ~/.config/zsh/reload/init.zsh
}

# Simple bat output with fallback to cat
function simple_bat {
    if ! [ -x "$(command -v bat)" ]; then
        cat $argv
    else
        bat --style=header-filename,header-filesize,snip,grid $argv
    fi
}

alias cat="simple_bat"
