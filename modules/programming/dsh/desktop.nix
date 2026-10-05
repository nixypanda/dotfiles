{
  lib,
  stdenvNoCC,
  fetchurl,
  unzip,
  version ? "0.2.0-rc.2",
}:

let
  system = stdenvNoCC.hostPlatform.system;

  # DeepSeek Harness Desktop is a signed, notarized Electron shell published
  # only as a prebuilt artifact; its source in the harness repo is not a
  # buildable release (private workspace packages plus release signing). The
  # download page points at fixed "latest" DMG URLs that roll in place, so pin
  # the versioned ZIP named by the update feed instead:
  #
  #   https://download.deepseek.com/dsh-desk/feeds/mac-<arch>/nightly-mac.yml
  #
  # That feed carries the exact version, artifact URL, and sha512 (base64),
  # which is already Nix's SRI spelling. To bump, update `version` in the
  # module and the per-arch hashes below from the feed.
  sources = {
    aarch64-darwin = {
      url = "https://download.deepseek.com/dsh-desk/bin/mac-arm64/deepseek-harness-${version}-mac-arm64.zip";
      hash = "sha512-BIC7PMWuEEzM7OuK8tMXQR0AdtVlYrSP1usHGWSAOhbxeXOzq2s1i71jZphMQSoQ/8tUQr55a158JW8xScseuA==";
    };
    x86_64-darwin = {
      url = "https://download.deepseek.com/dsh-desk/bin/mac-x64/deepseek-harness-${version}-mac-x64.zip";
      hash = "sha512-WgqK37vYCuS3r+cuu002cLPMY2h0hiF8FhOwIzv13FozSbb4spQ22VRb58FRggjDeVhdRXZT2Jm+f3TGWNG8cQ==";
    };
  };

  source = sources.${system} or (throw "dsh/desktop.nix: unsupported system ${system}");
in
stdenvNoCC.mkDerivation {
  pname = "deepseek-harness-desktop";
  inherit version;

  src = fetchurl {
    inherit (source) url hash;
  };

  nativeBuildInputs = [ unzip ];

  dontConfigure = true;
  dontBuild = true;
  dontUnpack = true;

  # The bundle is signed and notarized. stdenv's fixupPhase would rewrite
  # script shebangs (and could strip binaries), invalidating the signature and
  # the stapled ticket, so skip fixups entirely: the archive is already the
  # final artifact.
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    # The archive holds a signed `DeepSeek Harness.app`. Extracting preserves
    # bytes and symlinks, so its Developer ID signature stays valid; the store
    # fetch adds no quarantine xattr, so Gatekeeper never blocks the first
    # launch. Home Manager copies `$out/Applications` into
    # ~/Applications/Home Manager Apps.
    mkdir -p $out/Applications $out/bin
    unzip -q "$src" -d "$TMPDIR/unpack"

    app="$(find "$TMPDIR/unpack" -maxdepth 1 -name '*.app' -print -quit)"
    [ -n "$app" ] || {
      echo "deepseek-harness-desktop: no .app bundle found in archive" >&2
      exit 1
    }
    app_name="$(basename "$app")"
    cp -a "$app" "$out/Applications/$app_name"

    # Convenience CLI entry point. The shell bundle also ships its own packaged
    # `dsh` command under Contents/Resources/runtime/cli; this is just the app
    # binary.
    exe="$(find "$out/Applications/$app_name/Contents/MacOS" -maxdepth 1 -type f -perm -u+x -print -quit)"
    [ -n "$exe" ] || {
      echo "deepseek-harness-desktop: no executable in $app_name/Contents/MacOS" >&2
      exit 1
    }
    ln -s "$exe" "$out/bin/deepseek-harness-desktop"

    runHook postInstall
  '';

  meta = {
    description = "DeepSeek Harness desktop application";
    homepage = "https://www.deepseek.com/harness/";
    license = lib.licenses.mit;
    mainProgram = "deepseek-harness-desktop";
    platforms = [
      "aarch64-darwin"
      "x86_64-darwin"
    ];
  };
}
