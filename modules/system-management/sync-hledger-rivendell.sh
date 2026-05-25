#!/bin/sh
set -eu

source_dir="/Users/nixypanda/Library/Mobile Documents/com~apple~CloudDocs/Money/ledger"
years_dir="${source_dir}/journals/years"
remote="nixypanda@100.127.3.54"
remote_dir="/srv/hledger"
remote_paisa_dir="${remote_dir}/paisa"
remote_years_dir="${remote_dir}/journals/years"
ssh_key="/Users/nixypanda/.ssh/github-key"
ssh_cmd="ssh -i ${ssh_key} -o StrictHostKeyChecking=accept-new"
ssh_interactive_cmd="ssh -tt -i ${ssh_key} -o StrictHostKeyChecking=accept-new"

usage() {
  printf '%s\n' "usage: sync-hledger-rivendell [--years|--bootstrap|--all]"
  printf '%s\n' "  --years      sync only years/ (default)"
  printf '%s\n' "  --bootstrap  copy everything except years/"
  printf '%s\n' "  --all        run bootstrap, then years sync"
}

refresh_paisa() {
  for name in mine wife combined dummy; do
    ${ssh_cmd} "${remote}" "cd '${remote_paisa_dir}' && PATH=/run/current-system/sw/bin:\$PATH paisa --config 'paisa-${name}.yaml' update --journal"
  done

  ${ssh_interactive_cmd} "${remote}" "sudo systemctl restart paisa-mine paisa-wife paisa-combined paisa-dummy"
}

sync_bootstrap_files() {
  rsync -rltD --omit-dir-times --no-perms --no-owner --no-group --exclude '/journals/years/' -e "${ssh_cmd}" \
    "${source_dir}/" \
    "${remote}:${remote_dir}/"
}

sync_years_files() {
  rsync -rltD --delete --omit-dir-times --no-perms --no-owner --no-group -e "${ssh_cmd}" \
    "${years_dir}/" \
    "${remote}:${remote_years_dir}/"
}

case "${1:---years}" in
  --years)
    sync_years_files
    refresh_paisa
    ;;
  --bootstrap)
    sync_bootstrap_files
    refresh_paisa
    ;;
  --all)
    sync_bootstrap_files
    sync_years_files
    refresh_paisa
    ;;
  -h|--help)
    usage
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac
