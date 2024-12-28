$env.config.show_banner = false

$env.config.completions.algorithm = "fuzzy"
$env.config.completions.external.completer = {|spans|
	let carapace_completer = {|spans|
		carapace $spans.0 nushell ...$spans
		| from json
	}
	let zoxide_completer = {|spans|
		$spans | skip 1 | zoxide query -l $in | lines | where {|x| $x != $env.PWD}
	}

	let expanded_alias = scope aliases | where name == $spans.0 | get -i 0 | get -i expansion
	let spans = if $expanded_alias != null  {
		$spans | skip 1 | prepend ($expanded_alias | split row " " | take 1)
	} else {
		$spans
	}

	match $spans.0 {
		__zoxide_z | __zoxide_zi => $zoxide_completer,
		_ => $carapace_completer
	} | do $in $spans
}


$env.config.cursor_shape.vi_insert = "line"
$env.config.cursor_shape.vi_normal = "block"
$env.config.edit_mode = "vi"

# direnv setup
$env.config.hooks.pre_prompt = [{ ||
            if (which direnv | is-empty) {
                return
            }

            direnv export json | from json | default {} | load-env
        }]

# fuzzy history searching
$env.config.keybindings ++= [
        {
            name: fuzzy_history_fzf
            modifier: control
            keycode: char_r
            mode: [emacs , vi_normal, vi_insert]
            event: {
              send: executehostcommand
              cmd: "
                commandline edit -r (history 
                  | each { |it| $it.command } 
                  | uniq 
                  | reverse 
                  | str join (char -i 0) 
                  | fzf --read0 --tiebreak=chunk --layout=reverse  --multi --preview='bat --style=numbers {..}' --preview-window='bottom:hidden' --bind shift-tab:up,tab:down --height=50% -q (commandline)
                  | decode utf-8 
                  | str trim)
              "
            }
        }
]

source ~/.cache/nushell/carapace.nu
