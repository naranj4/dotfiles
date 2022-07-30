################################################################################
# ZSH Config File
################################################################################

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# load global zshrc
source ~/.config/zsh/init.zsh

# load local zshrc
if [ -f ~/.zshrc_local ]; then
    source ~/.zshrc_local
fi

eval "$(direnv hook zsh)"
