#!/bin/sh
set -eu

source_dir="/Users/nixypanda/Library/Mobile Documents/com~apple~CloudDocs/Money/ledger"
remote="nixypanda@srt-n01-rivendell"
remote_dir="/srv/hledger"
remote_paisa_dir="${remote_dir}/paisa"
ssh_key="/Users/nixypanda/.ssh/github-key"
ssh_cmd="ssh -i ${ssh_key} -o StrictHostKeyChecking=accept-new"
ssh_interactive_cmd="ssh -tt -i ${ssh_key} -o StrictHostKeyChecking=accept-new"

refresh_services() {
  ${ssh_cmd} "${remote}" "cd '${remote_paisa_dir}' && PATH=/run/current-system/sw/bin:\$PATH paisa --config 'paisa-mine.yaml' update --journal"

  ${ssh_interactive_cmd} "${remote}" "
    if systemctl cat hedger-mine.service >/dev/null 2>&1; then
      sudo systemctl reload hedger-mine
    fi
    sudo systemctl restart paisa-mine
  "
}

sync_tracked_files() {
  git -C "${source_dir}" ls-files |
    rsync -rltD --files-from=- --omit-dir-times --no-perms --no-owner --no-group -e "${ssh_cmd}" \
    "${source_dir}/" \
    "${remote}:${remote_dir}/"

  # Hedger runs as a separate user in the hledger group. Repair permissions
  # preserved by rsync, including private directories copied from iCloud.
  ${ssh_cmd} "${remote}" "
    set -eu
    find '${remote_dir}/journals' '${remote_dir}/prices' '${remote_dir}/hedger' \\
      -user nixypanda -exec chgrp hledger {} + -exec chmod g+rX {} +
    find '${remote_dir}/journals' '${remote_dir}/prices' '${remote_dir}/hedger' \\
      -user nixypanda -type d -exec chmod g+s {} +
  "
}

sync_tracked_files
refresh_services
