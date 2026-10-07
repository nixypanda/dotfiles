{ pkgs }:
# Build one immutable Vale styles tree from packaged style sets. Vale expects
# vocabulary files to exist, so create empty Base vocab files instead of
# relying on mutable setup under $HOME.
pkgs.runCommand "vale-styles"
  {
    buildInputs = with pkgs.valeStyles; [
      alex
      proselint
      write-good
    ];
  }
  ''
    mkdir -p $out/config/vocabularies/Base
    touch $out/config/vocabularies/Base/accept.txt
    touch $out/config/vocabularies/Base/reject.txt
    for pkg in $buildInputs; do
      cp -rs "$pkg/share/vale/styles/"* "$out/"
    done
  ''
