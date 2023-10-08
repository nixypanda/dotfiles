export def "loc tldr" [] {
    tokei -o json |
    from json |
    transpose |
    flatten |
    where column0 != "Total" |
    insert files {|row| $row.reports | length } |
    rename -c [column0 language] |
    insert lines {|row| $row.blanks + $row.code + $row.comments}
}

# Function to create a fixup commit and perform an interactive rebase
# Usage: gfix <commit_sha>
def gfix [commit_sha] {
    git commit --fixup $commit_sha
    git rebase -i --autosquash "$commit_sha~1"
}

# Function to create a fixup commit, stash changes, perform an interactive rebase,
# and pop the stashed changes
# Usage: gstashnfix <commit_sha>
def gstashnfix [commit_sha] {
    git commit --fixup $commit_sha
    git stash
    git rebase -i --autosquash "$commit_sha~1"
    git stash pop
}
