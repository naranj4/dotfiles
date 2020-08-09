#!/bin/zsh

# Install all necessary dependencies for dev environment
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Mac OS
    echo "============================"
    echo "Setting up MacOS environment"
    echo "============================"

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

    # guis
    brew cask install iterm2

    echo "[========SYM LINKING DOTFILES========]"
    # vim and neovim dotfiles
    ln -sfn ~/dotfiles/.vimrc ~/.vimrc
    mkdir -p ~/.config/nvim
    ln -sfn ~/dotfiles/.vimrc ~/.config/nvim/init.vim
    echo "\tLinked vim and neovim configs"
    # TODO: sym link the dotfiles over

else
    echo "Unsupported OS ${OSTYPE}. Manual installation required."
fi

echo "======================================"
echo "Finished development environment setup"
echo "======================================"
