set -eu

sessions_dir="$HOME/.local/state/kitty/sessions"
# No saved sessions yet; the keybinding should just do nothing.
[ -d "$sessions_dir" ] || exit 0

# @find@, @fzf@, @kitty@, and friends are replaced by Nix with store paths.
selection="$(
  @find@ "$sessions_dir" -type f -name "*.kitty-session" -print0 |
    @sort@ -z |
    while IFS= read -r -d '' file; do
      name="$(@basename@ "$file" .kitty-session)"
      printf '%s\t%s\n' "$name" "$file"
    done |
    @fzf@ \
      --prompt='kitty session> ' \
      --height=80% \
      --layout=reverse \
      --border \
      --with-nth=1 \
      --delimiter='\t'
)"
# Esc or an empty picker result should leave the current session unchanged.
[ -n "$selection" ] || exit 0

session_path="$(printf '%s' "$selection" | @cut@ -f2-)"

# Talks to the socket configured by allow_remote_control/listen_on in kitty.conf.
@kitty@ @ action goto_session "$session_path"
