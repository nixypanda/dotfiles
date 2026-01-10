$env.STARSHIP_SHELL = "nu"
$env.SHELL = "nu"

# https://github.com/nix-community/home-manager/issues/6507
# https://github.com/nushell/nushell/issues/8230
use std/util "path add"
path add $"($env.HOME)/.nix-profile/bin"
path add "/run/current-system/sw/bin"
path add "/nix/var/nix/profiles/default/bin"
path add "/usr/local/bin"


def create_left_prompt [] {
    starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
}

# Use nushell functions to define your right and left prompt
$env.PROMPT_COMMAND = { || create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = ""

# The prompt indicators are environmental variables that represent
# the state of the prompt
$env.PROMPT_INDICATOR = ""
$env.PROMPT_INDICATOR_VI_INSERT = ""
$env.PROMPT_INDICATOR_VI_NORMAL = ""
$env.PROMPT_MULTILINE_INDICATOR = ""
