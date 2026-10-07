{
  lib,
  stdenvNoCC,
  fetchurl,
  version ? "24.21.0",
}:

let
  system = stdenvNoCC.hostPlatform.system;

  # DSH's node-addon-require-builtin probes the running Node binary's machine
  # code for a specific getter pattern. nixpkgs builds Node with hardening flags
  # (-fno-omit-frame-pointer -mno-omit-leaf-frame-pointer on arm64,
  # -fzero-call-used-regs=used-gpr on x64) that change those bytes, so the probe
  # reports Unsupported/no-getter and DSH >= 0.1.6 fails to boot. The official
  # nodejs.org builds are the ones the addon recognises, so run DSH under this
  # instead of pkgs.nodejs. Version matches the Node DSH 0.2.0-rc.2 was
  # published with.
  #
  # To bump, refresh both hashes:
  #   nix store prefetch-file --json --hash-type sha256 \
  #     "https://nodejs.org/dist/v<version>/node-v<version>-darwin-<arch>.tar.gz"
  sources = {
    aarch64-darwin = {
      file = "node-v${version}-darwin-arm64.tar.gz";
      hash = "sha256-vtfupTJeEQjzLOUijd1qXw8IpJnuQqp0Qq6lg3AvYFc=";
    };
    x86_64-darwin = {
      file = "node-v${version}-darwin-x64.tar.gz";
      hash = "sha256-FGLLOzBGuBXPjqQ209pFDsGp8R2sflpGsK2lMF1+gJc=";
    };
  };

  source =
    sources.${system}
      or (throw "nodejs-official.nix: unsupported system ${system}; use pkgs.nodejs instead");
in
stdenvNoCC.mkDerivation {
  pname = "nodejs-official";
  inherit version;

  src = fetchurl {
    url = "https://nodejs.org/dist/v${version}/${source.file}";
    inherit (source) hash;
  };

  dontConfigure = true;
  dontBuild = true;
  dontUnpack = true;

  # Strip the node-v<version>-darwin-<arch>/ prefix and keep the layout:
  # bin/node, bin/npm, bin/npx, lib/node_modules/npm, include, share.
  installPhase = ''
    runHook preInstall

    mkdir -p $out
    tar xzf $src -C $out --strip-components=1

    runHook postInstall
  '';

  meta = {
    description = "Node.js (official nodejs.org build)";
    homepage = "https://nodejs.org";
    license = lib.licenses.mit;
    mainProgram = "node";
    platforms = [
      "aarch64-darwin"
      "x86_64-darwin"
    ];
  };
}
