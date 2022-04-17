echo ""
echo "[========SYM LINKING DOTFILES========]"
# vim and neovim dotfiles
ln -sfn ~/dotfiles/.vimrc ~/.vimrc
ln -sfn ~/dotfiles/nvim ~/.config/nvim
echo "Linked vim and neovim configs"

# tmux config
ln -sfn ~/dotfiles/.tmux.conf ~/.tmux.conf
echo "Linked tmux config"

# zsh config
ln -sfn ~/dotfiles/.zshrc ~/.zshrc
ln -sfn ~/dotfiles/.p10k.zsh ~/.p10k.zsh
ln -sfn ~/dotfiles/zsh ~/.config/zsh
echo "Linked zsh and p10k config"

# git config
ln -sfn ~/dotfiles/.gitconfig ~/.gitconfig
echo "Linked git config"

echo "DONE"
