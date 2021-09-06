# add Homebrew to path if linux-gnu system
if [[ "$OSTYPE" = "linux-gnu" ]]; then
    eval $($HOME/.linuxbrew/bin/brew shellenv)
fi
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

# setting terminal theme
ZSH_THEME="powerlevel10k/powerlevel10k"

# autosuggestions settings
source ~/.zplug/repos/zsh-users/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'
ZSH_AUTOSUGGEST_HISTORY_IGNORE="?(#c50,)"
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
bindkey '^ ' autosuggest-accept

# case insensitive tab completion
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
autoload -Uz compinit && compinit

# make nvim the default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# set quality of life aliases
alias ls="ls -GF"
alias ll="ls -l"
alias la="ls -la"
alias gs="git status"
alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gd="git diff"
alias gco="git checkout"
alias glg="git log --oneline"
alias ggr="git graph"
alias ghst="git hist"

# load local zshrc file
if [ -f ~/.zshrc_local ]; then
    echo "Loaded local zsh config"
    source ~/.zshrc_local
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

source /Users/niranjansankar/.config/broot/launcher/bash/br
