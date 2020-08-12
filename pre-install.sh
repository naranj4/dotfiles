#!/bin/bash

# script installs prerequisites before setting up homebrew
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    # Linux system
    if [ -f /etc/debian_version ]; then
        echo "Debian-based Linux system"
        sudo apt-get install build-essential curl file git
    else
        echo "RPM-based Linux system"
        sudo yum groupinstall 'Development Tools' && sudo yum install curl file git
    fi
fi
echo "Close your session and reopen terminal. Otherwise, homebrew will be installed in the /home/ directory"
