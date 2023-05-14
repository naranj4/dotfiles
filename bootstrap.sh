#!/bin/bash

# Install nvm and NodeJS (for LSPs)
if [[ ! -f "$HOME/.config/nvm/nvm.sh" ]]; then
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
fi
nvm install node --reinstall-packages-from=node

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
