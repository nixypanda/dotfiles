{
  # Enable Do-not-Track
  # With the do not track feature, you tell websites, that you do not want to be
  # tracked. Most websites ignore this, so you need other privacy options as well.
  "privacy.donottrackheader.enabled" = true;
  "privacy.donottrackheader.value" = 1;
  # Enable resistFingerprinting
  # The <code>privacy.resistFingerprinting</code> setting coming from the
  # tor-browser hides some system properties. See discussion in our bug tracker.
  # (https://bugzilla.mozilla.org/show_bug.cgi?id=1308340">Bug #1308340</a> for more
  # information. This option may interfere with other privacy related settings, see
  # the <a
  # href="https://github.com/allo-/firefox-profilemaker/issues/56#issuecomment-333397712)
  "privacy.resistFingerprinting" = false;
  # Enable Mozilla Trackingprotection
  # Firefox has a builtin tracking protection
  # (https://wiki.mozilla.org/Security/Tracking_protection), which blocks a list of
  # known tracking sites.
  "privacy.trackingprotection.pbmode.enabled" = true;
  "privacy.trackingprotection.enabled" = true;
  "privacy.trackingprotection.fingerprinting.enabled" = true;
  "privacy.trackingprotection.cryptomining.enabled" = true;
  # Enable firstparty isolation.
  # FPI works by separating cookies on a per-domain basis. In this way tracking
  # networks won't be able to locate the same cookie on different sites. Note that
  # this might break third-party logins.
  "privacy.firstparty.isolate" = false;
  # Disable Browser Pings
  # Firefox sends "ping" requests (http://kb.mozillazine.org/Browser.send_pings),
  # when a website requests to be informed when a user clicks on a link.
  # NOTE: This breaks google meet camera
  "browser.send_pings" = true;
  # Disable TLS session identifiers
  # TLS allows for session identifiers, which speed up the session resumption when a
  # connection was lost. These identifiers can be used for tracking
  # (https://youbroketheinternet.org/trackedanyway).
  "security.ssl.disable_session_identifiers" = true;
  # Disable Beacons
  # The Beacon (https://w3c.github.io/beacon/) feature allows websites to send
  # tracking data after you left the website.
  "beacon.enabled" = false;
  # Disable the Battery API
  # Firefox allows websites to read the charge level of the battery. This may be
  # used for fingerprinting.
  "dom.battery.enabled" = false;
  # Disable media device queries
  # Prevent websites from accessing information about webcam and microphone
  # (https://developer.mozilla.org/docs/Web/API/MediaDevices/enumerateDevices)
  # (possible fingerprinting).
  "media.navigator.enabled" = false;
  # Disable form autofill
  # Automatically filled form fields are used for fingerprinting
  # (https://freedom-to-tinker.com/2017/12/27/no-boundaries-for-user-identities-web-trackers-exploit-browser-login-managers/).
  # This setting disables automatic form filling until you click on the field.
  "signon.autofillForms" = false;
  # Disable webaudio API
  # Disable webaudio API to prevent browser fingerprinting. See Mozilla Bug #1288359
  # (https://bugzilla.mozilla.org/show_bug.cgi?id=1288359). This can break web apps,
  # like Discord, which rely on the API.
  "dom.webaudio.enabled" = false;
  # Disable video statistics
  # Prevent websites from measuring video performance (possible fingerprinting). See
  # Mozilla Bug 654550 (https://bugzilla.mozilla.org/show_bug.cgi?id=654550).
  "media.video_stats.enabled" = false;
  # Enable query parameter stripping
  # Firefox 102 introduced query parameter stripping like utm_source. Enabled by
  # default with Strict Enhanced Tracking Protection.
  "privacy.query_stripping" = true;

}
