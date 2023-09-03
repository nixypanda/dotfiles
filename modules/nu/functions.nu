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
