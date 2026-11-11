{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nix-update-script,
}:

buildNpmPackage rec {
  pname = "stremio-local-debrid";
  version = "1.2.0";

  # Tracks nixypanda's main branch, which carries configurable next-episode
  # prefetching and direct completed-cache playback on top of upstream 1.2.0.
  src = fetchFromGitHub {
    owner = "nixypanda";
    repo = "stremio-local-debrid";
    rev = "91493290c7eddcbe8671f143a5174fe8c2ce3330";
    hash = "sha256-7jTwqf+bOPgjKONDeagIJ8R7Wy54WxvNXAL8u3Bkz7U=";
  };

  npmDepsHash = "sha256-uMFR36ifIhxiQ0S7H/sLAMLpzdF6aYAo+dUK1Kilk0g=";

  # Dependencies include packages with install scripts that fetch their own
  # tooling (ip-set runs `npx only-allow pnpm`). The package is pure
  # JavaScript, so skip every lifecycle script.
  npmFlags = [ "--ignore-scripts" ];

  dontNpmBuild = true;

  # Read the config path from the environment so the NixOS module can keep
  # config.json (which contains the private access token) outside the store.
  postPatch = ''
    substituteInPlace server.mjs \
      --replace-fail \
        "JSON.parse(await readFile(join(root, 'config.json'), 'utf8'))" \
        "JSON.parse(await readFile(process.env.STREMIO_LOCAL_DEBRID_CONFIG || join(root, 'config.json'), 'utf8'))"
  '';

  # WebTorrent pulls in webrtc-polyfill, whose Node entry point loads the
  # native node-datachannel addon. This server only connects to ordinary
  # TCP/uTP BitTorrent peers, so replace the WebRTC implementation with a
  # stub and keep the package pure JavaScript.
  postInstall = ''
        webrtc="$out/lib/node_modules/${pname}/node_modules/webrtc-polyfill/index.js"
        if [ -f "$webrtc" ]; then
          cat > "$webrtc" <<'EOF'
    const unsupported = name => class {
      constructor() {
        throw new Error(name + ' is unavailable in this server build');
      }
    };
    export const RTCPeerConnection = unsupported('RTCPeerConnection');
    export const RTCSessionDescription = unsupported('RTCSessionDescription');
    export const RTCIceCandidate = unsupported('RTCIceCandidate');
    export const RTCIceTransport = unsupported('RTCIceTransport');
    export const RTCDataChannel = unsupported('RTCDataChannel');
    export const RTCSctpTransport = unsupported('RTCSctpTransport');
    export const RTCDtlsTransport = unsupported('RTCDtlsTransport');
    export const RTCCertificate = unsupported('RTCCertificate');
    export const RTCError = unsupported('RTCError');
    export const RTCRtpTransceiver = unsupported('RTCRtpTransceiver');
    export const RTCRtpSender = unsupported('RTCRtpSender');
    export const RTCRtpReceiver = unsupported('RTCRtpReceiver');
    export const MediaStream = unsupported('MediaStream');
    export const MediaStreamTrack = unsupported('MediaStreamTrack');
    export default {};
    EOF
        fi
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [ "--version=branch=main" ];
  };

  meta = {
    description = "Self-hosted Stremio torrent cache addon";
    homepage = "https://github.com/origami-ltd/stremio-local-debrid";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
