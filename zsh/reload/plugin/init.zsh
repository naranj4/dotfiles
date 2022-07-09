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

zplug "junegunn/fzf", use:"shell/*.zsh"

zplug load

################################################################################
# Plugin Configuration
################################################################################
source ~/.config/zsh/reload/plugin/config/autosuggest.zsh
source ~/.config/zsh/reload/plugin/config/vi-mode.zsh
