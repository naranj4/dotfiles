#!/bin/zsh

# Install all necessary dependencies for dev environment
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Mac OS
    echo "============================"
    echo "Setting up MacOS environment"
    echo "============================"

    echo "[========INSTALLING DEPENDENCIES========]"

    # install homebrew
    which -s brew
    if [[ $? != 0 ]]; then
        echo "Installing homebrew"
        /bin/bash -c \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    else
        echo "Updating homebrew"
        brew update
    fi
    
    # install brew formulae
    brew install git
    brew install python@3.8
    brew install the_silver_searcher
    brew install neovim
    brew install tmux

    # guis
    brew cask install iterm2

    brew upgrade

    which -s zplug
    if [[ $? != 0 ]]; then
        echo "Installing zplug"
        curl -sL --proto-redir \
            -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
    fi

    echo "[========SYM LINKING DOTFILES========]"
    # vim and neovim dotfiles
    ln -sfn ~/dotfiles/.vimrc ~/.vimrc
    mkdir -p ~/.config/nvim
    ln -sfn ~/dotfiles/.vimrc ~/.config/nvim/init.vim
    echo "\tLinked vim and neovim configs"

    # tmux config
    ln -sfn ~/dotfiles/.tmux.conf ~/.tmux.conf
    echo "\tLinked tmux config"

    # zsh config
    ln -sfn ~/dotfiles/.zshrc ~/.zshrc
    ln -sfn ~/dotfiles/.p10k.zsh ~/.p10k.zsh
    echo "\tLinked zsh and p10k config"

    # git config
    ln -sfn ~/dotfiles/.gitconfig ~/.gitconfig
    echo "\tLinked git config"

else
    echo "Unsupported OS [${OSTYPE}]. Manual installation required."
fi

echo "======================================"
echo "Finished development environment setup"
echo "======================================"
