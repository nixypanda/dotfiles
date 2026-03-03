set -eu

sessions_dir="$HOME/.local/state/kitty/sessions"
[ -d "$sessions_dir" ] || exit 0

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
[ -n "$selection" ] || exit 0

session_path="$(printf '%s' "$selection" | @cut@ -f2-)"

@kitty@ @ action goto_session "$session_path"
