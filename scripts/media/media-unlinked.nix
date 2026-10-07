{ pkgs }:
pkgs.writeShellApplication {
  name = "media-unlinked";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.findutils
  ];
  text = ''
    if [ "''${1:-}" = "-h" ] || [ "''${1:-}" = "--help" ]; then
      echo "Usage: media-unlinked [ROOT] [MIN_SIZE]"
      echo
      echo "List regular files with one hardlink and a size above MIN_SIZE."
      echo "ROOT defaults to /srv/media; MIN_SIZE defaults to 50M (50 MiB)."
      exit 0
    fi

    root="''${1:-/srv/media}"
    minimum_size="''${2:-50M}"

    if [ ! -d "$root" ]; then
      echo "media-unlinked: directory does not exist: $root" >&2
      exit 1
    fi

    printf "%-10s\t%-14s\t%-5s\t%-10s\t%-16s\t%-16s\t%-10s\t%-12s\t%-25s\t%s\n" \
      SIZE BYTES LINKS MODE OWNER GROUP DEVICE INODE MODIFIED PATH

    find "$root" -xdev \
      -path "$root/lost+found" -prune -o \
      -type f -size "+$minimum_size" -links 1 \
      -printf '%s\t%n\t%M\t%u\t%g\t%D\t%i\t%TY-%Tm-%TdT%TH:%TM:%TS%Tz\t%p\n' \
      | sort --numeric-sort --reverse \
      | while IFS=$'\t' read -r bytes links mode owner group device inode modified path; do
        human_size=$(numfmt --to=iec-i --suffix=B "$bytes")
        printf "%-10s\t%-14s\t%-5s\t%-10s\t%-16s\t%-16s\t%-10s\t%-12s\t%-25s\t%s\n" \
          "$human_size" "$bytes" "$links" "$mode" "$owner" "$group" \
          "$device" "$inode" "$modified" "$path"
      done
  '';
  meta.description = "List large files in a media tree that have no hardlinks";
}
