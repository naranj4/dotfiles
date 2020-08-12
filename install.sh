#!/bin/bash

# TODO: add wrappers around each install to doublecheck whether they have already been installed.
# Prompt user whether they desire to install the packages if so.

# Install all necessary dependencies for dev environment
if [[ "$OSTYPE" == "darwin"* ]] || \
    [[ "$OSTYPE" == "linux-gnu" ]]
then
    echo "========================================================"
    echo "Setting up development environment for $OSTYPE"
    echo "========================================================"

    echo "[========INSTALLING DEPENDENCIES========]"

    # install homebrew
    which brew
    if [[ $? != 0 ]]; then
        echo "Installing homebrew"
        /bin/bash -c \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
        # add Homebrew to path if linux-gnu system
        if [[ "$OSTYPE" = "linux-gnu" ]]; then
            echo "Detected Linux env. Running the following commands"
            $HOME/.linuxbrew/bin/brew shellenv
            echo ""
            eval $($HOME/.linuxbrew/bin/brew shellenv)
        fi
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
    brew install node
    brew install yarn

    # install zsh
    which zsh
    if [[ $? != 0 ]]; then
        brew install zsh
    fi

    # guis
    if [[ "$OSTYPE" == "darwin"* ]]; then
        brew cask install iterm2
    fi

    brew upgrade

    which zplug
    if [[ $? != 0 ]]; then
        echo "Installing zplug"
        curl -sL --proto-redir \
            -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
    fi

    echo "Install/upgrading pip"
    pip3 install -U pip
    echo "Install/upgrading jedi-language-server"
    pip3 install -U jedi-language-server

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

    echo "======================================"
    echo "Finished development environment setup"
    echo "======================================"

else
    echo "Unsupported OS [${OSTYPE}]. Manual installation required."
fi
