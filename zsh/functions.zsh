function dce() {
    docker-compose exec $1 /bin/bash -c "$2"
}

function dcpytest() {
    docker-compose exec $1 /bin/bash -c \
        'pytest'
}

function dcpytestcov() {
    docker-compose exec $1 /bin/bash -c \
        'pytest --cov='$2
}

function dcpytestcovhtml() {
    docker-compose exec $1 /bin/bash -c \
        'pytest --cov-report html --cov='$2
}

function dcpytestlf() {
    docker-compose exec $1 /bin/bash -c \
        'pytest --lf'
}

function dcpytestni() {
    docker-compose exec $1 /bin/bash -c \
        "pytest -m 'not integration'"
}

function dcpytestnicov() {
    docker-compose exec $1 /bin/bash -c \
        "pytest --cov='$2' -m 'not integration'"
}

function dcpytestnicovhtml() {
    docker-compose exec $1 /bin/bash -c \
        "pytest -m 'not integartion' --cov-report html --cov='$2'"
}

function gfix() {
    git commit --fixup=$1
    git rebase -i --autosquash $1~1
}

function gstashnfix() {
    git commit --fixup=$1
    git stash
    git rebase -i --autosquash $1~1
    git stash pop
} 

function git-save-my-secret-commiting-ass() {
    git filter-branch --force --index-filter \
        "git rm --cached --ignore-unmatch $1" \
         --prune-empty --tag-name-filter cat -- --all

    echo $1 >> .gitignore

    git push origin --force --all
    git push origin --force --tags

    git for-each-ref --format="delete %(refname)" refs/original | git update-ref --stdin
    git reflog expire --expire=now --all
    git gc --prune=now
}

# Clean up python project of cache and stuff
function pyclean () {
    find . -type f -name '*.py[co]' -delete -o -type d -name __pycache__ -delete
}

# Recursive execution
function recursive() {
    for d in ./*/ ; do /home/sherub/.nix-profile/bin/zsh -c "(cd "$d" && "$@")"; done
}

function recursivep() {
    for d in ./*/ ; do /home/sherub/.nix-profile/bin/zsh -c "(cd "$d" && "$@") &"; done
}
