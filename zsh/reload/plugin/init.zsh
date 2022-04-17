################################################################################
# ZSH Plugins
################################################################################
source ~/.zplug/init.zsh

# zplug manages itself
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# git support
zplug "plugins/git", from:oh-my-zsh

# themes
zplug "romkatv/powerlevel10k", as:theme, depth:1

# syntax highlighting and autosuggestions
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-autosuggestions"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load --verbose

################################################################################
# Plugin Configuration
################################################################################
source ~/.config/zsh/reload/plugin/config/autosuggest.zsh
