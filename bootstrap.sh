#!/bin/bash

# Allows comments in the package lists.
function list-packages {
    cat $* | sed 's/#.*//' | grep "\S"
}

# Install nvm and NodeJS (for LSPs)
if [[ ! -f "$HOME/.config/nvm/nvm.sh" ]]; then
    PROFILE=/dev/null bash -c 'curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash'
fi

bash -c "\\. \"$NVM_DIR/nvm.sh\"; \
    nvm install node --reinstall-packages-from=default; \
    nvm alias default node"

# Install cargo packages
if ! command -v cargo; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

# TODO: fix this later when reorganizing dotfiles
# Create symlinks for config files, backing up the old ones.
# for config_file in $(find -L "${configroot}" -type f); do
#    target_file=$(echo "${config_file}" | sed 's/.*\/config\//\/home\/nisankar\//')
#     echo $target_file
#     if [[ -h "${target_file}" ]]; then
#        unlink "${target_file}"
#    fi
#     if [[ -f "${target_file}" ]]; then
#        mv "${target_file}" "${target_file}.backup"
#    fi
#     dir=$(dirname "${target_file}")
#    mkdir -p "$dir"
#    ln -s "${config_file}" "${target_file}"
# done
