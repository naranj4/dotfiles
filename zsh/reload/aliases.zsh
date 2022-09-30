################################################################################
# ZSH Aliases
################################################################################

# list files
alias ls="ls -GF"
alias ll="ls -l"
alias la="ls -la"

# Git aliases
alias gs="git status"
alias gd="git diff"

alias stash="git stash"

alias gp="git pull"

alias ga="git add"

alias gc="git commit"
alias gcm="git commit -m"
alias gca="git commit --amend"
alias gcan="git commit --amend --no-edit"
alias gcf="git commit --fixup"

alias gr="git rebase"
alias grf="git rebase --autosquash -i"
alias gfm="git branch --set-upstream-to=origin/\`git remote show origin | sed -n '/HEAD branch/s/.*: //p'\`"

alias gb="git branch"
alias gco="git checkout"
alias gcb="git checkout -b"

alias glg="git log --oneline"
alias ggr="git graph"
alias ghst="git hist"

alias gmt="git mergetool"
