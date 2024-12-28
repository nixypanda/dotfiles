use std

# crashes
# Error: nu::parser::parse_mismatch
# 
#   × Parse mismatch during operation.
#    ╭─[/nix/store/kiawjm5hlcn8rkg0gd75g6025n6nqs5f-functions.nu:7:1]
#  7 │     insert files {|row| $row.reports | length } |
#  8 │     rename -c [column0 language] |
#    ·               ─────────┬────────
#    ·                        ╰── expected non-[] value
#  9 │     insert lines {|row| $row.blanks + $row.code + $row.comments}
#    ╰────
# 
# export def "loc tldr" [] {
#     tokei -o json |
#     from json |
#     transpose |
#     flatten |
#     where column0 != "Total" |
#     insert files {|row| $row.reports | length } |
#     rename -c [column0 language] |
#     insert lines {|row| $row.blanks + $row.code + $row.comments}
# }

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
    | filter {($in | str length) > 0} 
    | parse "{key}={value}"
    | update value {str trim -c '"'}
    | transpose -r -d
}

def "from mypy" [] {
    lines
      | parse "{path}:{line}: {level}: {message} [{type}]" | upsert line {|row| $row.line | into int }
}

def "from flake8" [] {
    lines
      | parse "{path}:{line}:{column}: {code} {message}"
      | upsert line {|row| ($row.line | into int) }
      | upsert column {|row| ($row.column | into int) }
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

# Show some history stats similar to how atuin does it
def history-stats [
  --summary (-s): int = 10
  --last-cmds (-l): int
] {
  let top_commands = (
    history
    | if ($last_cmds != null) { last $last_cmds } else { $in }
    | get command
    | split column ' ' command
    | uniq -c
    | flatten
    | sort-by --reverse count
    | first $summary
  )

  let total_cmds = (history | length)
  let unique_cmds = (history | get command | uniq | length)

  print $"Top (ansi green)($summary)(ansi reset) most used commands:"
  let max = ($top_commands | get count | math max)
  $top_commands | each {|cmd|
    let in_ten = 10 * ($cmd.count / $max)
    print -n "["
    print -n (ansi red)
    for i in 0..<$in_ten {
      if $i == 2 {
        print -n (ansi yellow)
      } else if $i == 5 {
        print -n (ansi green)
      }
      if $i != 10 {
        print -n "▮"
      }
    }
    for x in $in_ten..<10 {
      if $x < 9 {
        print -n " "
      }
    }
    print $"(ansi reset)] (ansi xterm_grey)($cmd.count  | fill -a r -c ' ' -w 4)(ansi reset) (ansi default_bold)($cmd.command)(ansi reset)"
  }

  print $"(ansi green)Total commands:(ansi reset)   ($total_cmds)"
  print $"(ansi green)Unique commands:(ansi reset)  ($unique_cmds)"
}

def is-ruff-pylint-yet [--detailed] {
    def _how_much_is_done [data]  {
        $data
        | parse "- [{exists}] {feature}"
        | histogram exists
        | filter {|row| $row.exists == "x"}
        | get percentage
    }

    let issue_str = curl -s https://api.github.com/repos/astral-sh/ruff/issues/970
    | from json
    | get body

    let issue_structured = $issue_str | lines | filter { |row| $row != "" }
    let split_by_type = $issue_structured | split list -r '^#' | skip 1
    let types = $issue_structured | filter { |row| $row | str starts-with "#" } | str trim --char '#' | str trim

    if $detailed {
        let completion_by_type = $split_by_type | each { |row| _how_much_is_done $row} | flatten | wrap "%"
        $types | wrap types | merge $completion_by_type
    } else {
        $split_by_type | flatten | _how_much_is_done $in | get 0
    }

}
