echo ""
echo "[========SYM LINKING DOTFILES========]"
# vim and neovim dotfiles
ln -sfn ~/dotfiles/.vimrc ~/.vimrc
mkdir -p ~/.config/nvim
ln -sfn ~/dotfiles/.vimrc ~/.config/nvim/init.vim
ln -sfn ~/dotfiles/coc-settings.json ~/.config/nvim/coc-settings.json
echo "Linked vim, neovim and coc configs"

# tmux config
ln -sfn ~/dotfiles/.tmux.conf ~/.tmux.conf
echo "Linked tmux config"

# zsh config
ln -sfn ~/dotfiles/.zshrc ~/.zshrc
ln -sfn ~/dotfiles/.p10k.zsh ~/.p10k.zsh
echo "Linked zsh and p10k config"

# git config
ln -sfn ~/dotfiles/.gitconfig ~/.gitconfig
echo "Linked git config"

echo "DONE"
