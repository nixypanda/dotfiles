{
  fetchzip,
  installShellFiles,
  lib,
  makeWrapper,
  stdenvNoCC,
  versionCheckHook,
}:

let
  version = "0.155.1";

  # Prebuilt release archives, keyed by host platform.
  platformSources = {
    x86_64-darwin = {
      url = "https://github.com/openai/codex/releases/download/rust-v${version}/codex-package-x86_64-apple-darwin.tar.gz";
      hash = "sha256-bPOcSuSn9DTflW8Y+6zqnGoNKid+apzmTxR1JzR6BTM=";
    };
    aarch64-darwin = {
      url = "https://github.com/openai/codex/releases/download/rust-v${version}/codex-package-aarch64-apple-darwin.tar.gz";
      hash = "sha256-xvNrGiVBenuiDU+gqVmvqW5EIWhFObtMXIxofUnOOvE=";
    };
  };

  source =
    platformSources.${stdenvNoCC.hostPlatform.system}
      or (throw "codex-bin: unsupported platform ${stdenvNoCC.hostPlatform.system}");
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "codex";
  inherit version;

  src = fetchzip (source // { stripRoot = false; });

  nativeBuildInputs = [
    installShellFiles
    makeWrapper
  ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out"
    cp -R . "$out"
    wrapProgram "$out/bin/codex" --prefix PATH : "$out/codex-path"

    installShellCompletion --cmd codex \
      --bash <("$out/bin/codex" completion bash) \
      --fish <("$out/bin/codex" completion fish) \
      --zsh <("$out/bin/codex" completion zsh)

    runHook postInstall
  '';

  doInstallCheck = true;
  nativeInstallCheckInputs = [ versionCheckHook ];

  meta = {
    description = "Lightweight coding agent that runs in your terminal";
    homepage = "https://github.com/openai/codex";
    changelog = "https://github.com/openai/codex/releases/tag/rust-v${finalAttrs.version}";
    license = lib.licenses.asl20;
    mainProgram = "codex";
    platforms = builtins.attrNames platformSources;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
})
