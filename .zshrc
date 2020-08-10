source ~/.zplug/init.zsh

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

# setting terminal theme
ZSH_THEME="powerlevel10k/powerlevel10k"

# autosuggestions settings
ZSH_AUTOSUGGEST_HISTORY_IGNORE = "?(#c50,)"
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE = 20
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
bindkey '^ ' autosuggest-accept

# make nvim the default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
