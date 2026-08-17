{
  fetchzip,
  installShellFiles,
  lib,
  makeWrapper,
  stdenvNoCC,
  versionCheckHook,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "codex";
  version = "0.146.0";

  src = fetchzip {
    url = "https://github.com/openai/codex/releases/download/rust-v${finalAttrs.version}/codex-package-x86_64-apple-darwin.tar.gz";
    hash = "sha256-2u1AePbX1+/ZKd/F6p4B3Fszo6C1A8zsWUgoR7fTVT4=";
    stripRoot = false;
  };

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
    platforms = [ "x86_64-darwin" ];
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
})
