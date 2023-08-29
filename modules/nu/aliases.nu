## ALIASES ##

# I don't like the default but my hand just types it
alias :q = exit
alias vi = nvim
alias vim = nvim
alias top = btm
alias htop = btm
alias ytop = btm
alias cat = bat

# docker-compose
alias dc = docker-compose
alias dcu = docker-compose up --build
alias dcl = docker-compose logs -f
alias dcisolated = docker-compose up --build --no-deps consul_common common-redis common_db

# Navigation
alias .. = cd ..
alias ... = cd ../..
alias .... = cd ../../..
alias ..... = cd ../../../..

# git
alias gl = git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit
alias gc = git commit
alias gco = git checkout
alias gs = git status
alias gca = git commit --amend
alias gd = git diff
alias gdc = git diff --cached
alias gir = git rebase -i
alias gpr = gh pr create
alias gdpr = gh pr create --draft
alias gppr = git push origin HEAD and gh pr create --fill
alias gsur = git submodule update --remote
