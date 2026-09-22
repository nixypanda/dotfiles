$env.STARSHIP_SHELL = "nu"
$env.SHELL = "nu"

$env.XDG_CONFIG_HOME = $"($env.HOME)/.config"
$env.XDG_CACHE_HOME = $"($env.HOME)/.cache"
$env.XDG_DATA_HOME = $"($env.HOME)/.local/share"
$env.XDG_STATE_HOME = $"($env.HOME)/.local/state"
$env.XDG_RUNTIME_DIR = $"($env.TMPDIR)"

# Home Manager only exports this via hm-session-vars.sh, which nushell never
# sources (issue 6507 below), so programs.claude-code.configDir has no effect
# without it. Set only this one variable: sourcing hm-session-vars.sh wholesale
# would also set CODEX_HOME, and Codex keeps its file-based auth.json and seven
# sqlite databases in ~/.codex, so relocating it logs you out.
$env.CLAUDE_CONFIG_DIR = $"($env.XDG_CONFIG_HOME)/claude"

# https://github.com/nix-community/home-manager/issues/6507
# https://github.com/nushell/nushell/issues/8230
use std/util "path add"
# `~/.nix-profile` is the stable symlink to the active Nix profile. The XDG
# path is where standalone/Determinate Nix actually stores it (`profiles`,
# plural). Add both so this works regardless of the Nix installation.
path add $"($env.HOME)/.nix-profile/bin"
path add $"($env.XDG_STATE_HOME)/nix/profiles/profile/bin"
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
