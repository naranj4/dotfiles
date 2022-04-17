################################################################################
# ZSH Config File
################################################################################
# load global zshrc
source ~/.config/zsh/init.zsh

# load local zshrc
if [ -f ~/.zshrc_local ]; then
    echo "Loaded local zsh config"
    source ~/.zshrc_local
fi
