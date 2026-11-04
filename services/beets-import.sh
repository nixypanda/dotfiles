#!/usr/bin/env bash

# Import finished MeTube audio downloads into the Beets library one file at a
# time. The service supplies the paths below so this script can also be run by
# hand with the same environment.

set -euo pipefail
umask 0002

inbox="${BEETS_INBOX:?BEETS_INBOX must be set}"
review="${BEETS_REVIEW:?BEETS_REVIEW must be set}"
state="${BEETS_STATE:?BEETS_STATE must be set}"
config="${BEETS_CONFIG:-/etc/beets/config.yaml}"

runs="$state/runs.jsonl"
timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
started_at="$(date +%s)"
run_log=""
summary_written=0
leftover_dir=""
failed_dir=""
orphan_dir=""
imported=0
skipped=0
failures=0
singles=0

mkdir -p -- "$state" "$review/leftover" "$review/failed" "$review/orphan-art"

note() {
  if [ -n "$run_log" ]; then
    printf '%s\n' "$*" >>"$run_log"
  else
    printf '%s\n' "$*" >&2
  fi
}

trim_run_history() {
  local size lines keep tmp

  [ -f "$runs" ] || return 0
  size="$(wc -c <"$runs")"
  lines="$(wc -l <"$runs")"
  size="${size//[[:space:]]/}"
  lines="${lines//[[:space:]]/}"

  if [ "$lines" -gt 2000 ]; then
    keep=2000
  elif [ "$size" -gt 1048576 ]; then
    keep=1000
  else
    return 0
  fi

  tmp="$runs.tmp.$$"
  if tail -n "$keep" -- "$runs" >"$tmp"; then
    mv -- "$tmp" "$runs"
  else
    rm -f -- "$tmp"
  fi
}

housekeeping() {
  # Keep one import log per run for two weeks.
  find "$state" -maxdepth 1 -type f -name 'import-*.log' -mtime +14 -delete \
    2>/dev/null || true
  trim_run_history
}

write_summary() {
  local exit_status=$?
  local finished_at duration status

  if [ "$summary_written" -eq 1 ] || [ -z "$run_log" ]; then
    return 0
  fi
  summary_written=1

  if [ "$exit_status" -ne 0 ] && [ "$failures" -eq 0 ]; then
    failures=1
  fi

  finished_at="$(date +%s)"
  duration=$((finished_at - started_at))
  if [ "$failures" -gt 0 ]; then
    status="error"
  elif [ "$skipped" -gt 0 ]; then
    status="partial"
  else
    status="ok"
  fi

  printf '{"timestamp":"%s","duration_seconds":%d,"albums":0,"singles":%d,"imported":%d,"skipped":%d,"failures":%d,"status":"%s","log":"%s"}\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$duration" "$singles" \
    "$imported" "$skipped" "$failures" "$status" "$run_log" >>"$runs"
  trim_run_history
}

move_to_review() {
  local kind="$1"
  local root="$2"
  local item="$3"
  local relative="${item#"$inbox"/}"
  local destination="$root/$relative"

  mkdir -p -- "$(dirname -- "$destination")"
  printf 'moving %s %s to %s\n' "$kind" "$item" "$destination" >>"$run_log"
  if ! mv -n -- "$item" "$destination"; then
    printf 'failed to move %s %s\n' "$kind" "$item" >>"$run_log"
    return 1
  fi
  if [ -e "$item" ] || [ -L "$item" ]; then
    printf '%s destination already exists for %s\n' "$kind" "$item" >>"$run_log"
    return 1
  fi

  return 0
}

move_leftover() {
  move_to_review leftover "$leftover_dir" "$1"
}

move_failed() {
  move_to_review failed "$failed_dir" "$1"
}

audio_is_valid() {
  local file="$1"
  local codec

  codec="$(ffprobe -v error -select_streams a:0 \
    -show_entries stream=codec_type -of csv=p=0 "$file" 2>/dev/null)" || return 1
  [ "$codec" = "audio" ]
}

# MeTube may leave WebM audio or WAV containers that Beets cannot import
# directly. Convert those in place and print the path Beets should import.
normalize_audio() {
  local source="$1"
  local target=""

  case "${source,,}" in
  *.webm)
    target="${source%.*}.converted.opus"
    if [ -e "$target" ]; then
      rm -f -- "$target"
    fi
    if ! ffmpeg -nostdin -loglevel error -y -i "$source" -vn \
      -c:a libopus -b:a 160k "$target" >>"$run_log" 2>&1; then
      rm -f -- "$target"
      return 1
    fi
    ;;
  *.wav)
    target="${source%.*}.converted.flac"
    if [ -e "$target" ]; then
      rm -f -- "$target"
    fi
    if ! ffmpeg -nostdin -loglevel error -y -i "$source" -vn \
      -c:a flac "$target" >>"$run_log" 2>&1; then
      rm -f -- "$target"
      return 1
    fi
    ;;
  *)
    printf '%s\n' "$source"
    return 0
    ;;
  esac

  if ! rm -f -- "$source"; then
    rm -f -- "$target"
    return 1
  fi

  printf '%s\n' "$target"
}

move_orphan_images() {
  local candidate candidate_name stem key relative destination
  local -A non_image_stems=()

  # Include every non-image file, even unstable or partial downloads, so an
  # image is kept while any same-stem sidecar or media file still exists.
  while IFS= read -r -d '' candidate; do
    candidate_name="${candidate##*/}"
    stem="${candidate_name%.*}"
    key="$(dirname -- "$candidate")/$stem"
    non_image_stems["$key"]=1
  done < <(
    find "$inbox" -type f ! -name '.*' \
      ! \( -iname '*.jpeg' -o -iname '*.jpg' -o -iname '*.png' \
      -o -iname '*.webp' \) \
      -print0
  )

  while IFS= read -r -d '' candidate; do
    candidate_name="${candidate##*/}"
    stem="${candidate_name%.*}"
    key="$(dirname -- "$candidate")/$stem"
    if [ -n "${non_image_stems[$key]:-}" ]; then
      continue
    fi

    relative="${candidate#"$inbox"/}"
    destination="$orphan_dir/$relative"
    mkdir -p -- "$(dirname -- "$destination")"
    note "moving orphan image $candidate to $destination"
    if ! mv -n -- "$candidate" "$destination"; then
      note "failed to move orphan image $candidate"
    fi
  done < <(
    find "$inbox" -type f ! -name '.*' -mmin +2 \
      \( -iname '*.jpeg' -o -iname '*.jpg' -o -iname '*.png' \
      -o -iname '*.webp' \) \
      -print0
  )
}

remove_old_empty_dirs() {
  # MeTube may still be using a playlist directory, so only remove directories
  # that have stayed empty for a full day.
  find "$inbox" -mindepth 1 -depth -type d -empty -mmin +1440 -delete \
    2>/dev/null || true
}

trap write_summary EXIT

if [ ! -d "$inbox" ]; then
  printf 'inbox does not exist: %s\n' "$inbox" >&2
  exit 1
fi

exec 9>"$state/import.lock"
if ! flock -n 9; then
  exit 0
fi

housekeeping

stable_audio=()
while IFS= read -r -d '' candidate; do
  stable_audio+=("$candidate")
done < <(
  find "$inbox" -type f ! -name '.*' -mmin +2 \
    \( -iname '*.aac' -o -iname '*.flac' -o -iname '*.m4a' \
    -o -iname '*.mka' -o -iname '*.mp3' -o -iname '*.oga' \
    -o -iname '*.ogg' -o -iname '*.opus' -o -iname '*.wav' \
    -o -iname '*.webm' \) \
    -print0 | sort -z
)

if [ "${#stable_audio[@]}" -eq 0 ]; then
  orphan_dir="$review/orphan-art/$timestamp"
  if [ -e "$orphan_dir" ]; then
    orphan_dir="$orphan_dir-$$"
  fi
  move_orphan_images
  remove_old_empty_dirs
  exit 0
fi

run_log="$state/import-$timestamp-$$.log"
: >"$run_log"
leftover_dir="$review/leftover/$timestamp"
failed_dir="$review/failed/$timestamp"
orphan_dir="$review/orphan-art/$timestamp"
if [ -e "$leftover_dir" ]; then
  leftover_dir="$leftover_dir-$$"
fi
if [ -e "$failed_dir" ]; then
  failed_dir="$failed_dir-$$"
fi
if [ -e "$orphan_dir" ]; then
  orphan_dir="$orphan_dir-$$"
fi

for file in "${stable_audio[@]}"; do
  singles=$((singles + 1))
  printf '\n=== %s ===\n' "$file" >>"$run_log"

  if ! import_path="$(normalize_audio "$file")"; then
    failures=$((failures + 1))
    printf 'conversion failed for %s\n' "$file" >>"$run_log"
    if [ -e "$file" ] || [ -L "$file" ]; then
      if ! move_failed "$file"; then
        failures=$((failures + 1))
      fi
    fi
    continue
  fi

  status=0
  beet -c "$config" import -q -s -m -w -P -I \
    --quiet-fallback=asis "$import_path" >>"$run_log" 2>&1 || status=$?

  if [ "$status" -ne 0 ]; then
    failures=$((failures + 1))
    printf 'import failed for %s (status %d)\n' "$import_path" "$status" >>"$run_log"
    if [ -e "$import_path" ] || [ -L "$import_path" ]; then
      if ! move_failed "$import_path"; then
        failures=$((failures + 1))
      fi
    fi
  elif [ -e "$import_path" ] || [ -L "$import_path" ]; then
    if audio_is_valid "$import_path"; then
      skipped=$((skipped + 1))
      printf 'import left valid file %s in the inbox (skipped or duplicate)\n' \
        "$import_path" >>"$run_log"
      if ! move_leftover "$import_path"; then
        failures=$((failures + 1))
      fi
    else
      failures=$((failures + 1))
      printf 'import left invalid audio %s in the inbox\n' "$import_path" >>"$run_log"
      if ! move_failed "$import_path"; then
        failures=$((failures + 1))
      fi
    fi
  else
    imported=$((imported + 1))
  fi
done

move_orphan_images
remove_old_empty_dirs

if [ "$failures" -gt 0 ]; then
  exit 1
fi
