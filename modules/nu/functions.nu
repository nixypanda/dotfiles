use std

def _create_bar [percentage] {
    let max_length = 20   # Length of the bar
    let filled_length = ($max_length * $percentage / 100) | into int

    let filled_bar =  "█" | std repeat $filled_length | str join
    let empty_bar =  "░" | std repeat ($max_length - $filled_length) | str join
    let bar = $filled_bar + $empty_bar
    $bar
}

def _into-filesize [in_half_kb] {
    let f_size = $in_half_kb * 512 |  into filesize 
    $f_size
}

def df [] {
    ^df -Y | jc --df | from json |
    | update 512_blocks { |row| _into-filesize $row.512_blocks }
    | update used { |row| _into-filesize $row.used }
    | update available { |row| _into-filesize $row.available }
    | insert capacity_bar { |row| _create_bar $row.capacity_percent}
    | rename -c {capacity_percent: "capacity (%)"}
    | rename -c {iused_percent: "iused (%)"}
}

# Function to create a fixup commit and perform an interactive rebase
# Usage: gfix <commit_sha>
def gfix [commit_sha] {
    git commit --fixup $commit_sha
    git rebase -i --autosquash $"($commit_sha)~1"
}

# Function to create a fixup commit, stash changes, perform an interactive rebase,
# and pop the stashed changes
# Usage: gstashnfix <commit_sha>
def gstashnfix [commit_sha] {
    git commit --fixup $commit_sha
    git stash
    git rebase -i --autosquash $"($commit_sha)~1"
    git stash pop
}

def "from env" []: string -> record {
  lines 
    | split column '#' 
    | get column1 
    | where {($in | str length) > 0} 
    | parse "{key}={value}"
    | update value {str trim -c '"'}
    | transpose -r -d
}

def "from ifconfig" []: string -> record {
    jc --ifconfig | from json | each { |entry| $entry | transpose | rename key value }
}

def "from dig" []: string -> record {
    jc --dig | from json | transpose | table -e
}

def whatismyip [] {
    curl -s ipinfo.io/what-is-my-ip | from json
}

def wc-parse [] {
  parse -r '(?P<lines>\d+)\s+(?P<words>\d+)\s+(?P<bytes>\d+)\s+(?P<file>.+)'
  | update lines { into int }
  | update words { into int }
  | update bytes { into int }
}

def wc-table [files?: list<string>] {
  if ($files | is-empty) {
    wc | wc-parse
  } else {
    $files | each {|f| wc $f } | wc-parse
  }
}
