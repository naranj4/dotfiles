################################################################################
# ZSH Autosuggest Config
################################################################################

source ~/.zplug/repos/zsh-users/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'
ZSH_AUTOSUGGEST_HISTORY_IGNORE="?(#c50,)"
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
bindkey '^ ' autosuggest-accept
