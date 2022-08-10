{

  # Annoyances

  # Disable firefox intro tabs on the first start
  # Disable the first run tabs with advertisements for the latest firefox features.
  "browser.startup.homepage_override.mstone" = "ignore";
  # Disable new tab page intro
  # Disable the intro to the newtab page on the first run
  "browser.newtabpage.introShown" = true;
  # Pocket Reading List
  # No details
  "extensions.pocket.enabled" = false;
  "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
  # Disable Sponsored Top Sites
  # Firefox 83 introduced sponsored top sites
  # (https://support.mozilla.org/en-US/kb/sponsor-privacy), which are sponsored ads
  # displayed as suggestions in the URL bar.
  "services.sync.prefs.sync.browser.newtabpage.activity-stream.showSponsoredTopSite" = false;
  # Disable about:config warning.
  # No details
  "browser.aboutConfig.showWarning" = false;
  # Do not trim URLs in navigation bar
  # By default Firefox trims many URLs (hiding the http:// prefix and trailing slash
  # /).
  "browser.urlbar.trimURLs" = false;
  # Disable checking if Firefox is the default browser
  # No details
  "browser.shell.checkDefaultBrowser" = false;
  # Disable reset prompt.
  # When Firefox is not used for a while, it displays a prompt asking if the user
  # wants to reset the profile. (see Bug #955950
  # (https://bugzilla.mozilla.org/show_bug.cgi?id=955950)).
  "browser.disableResetPrompt" = true;
  # Disable Heartbeat Userrating
  # With Firefox 37, Mozilla integrated the Heartbeat
  # (https://wiki.mozilla.org/Advocacy/heartbeat) system to ask users from time to
  # time about their experience with Firefox.
  "browser.selfsupport.url" = "";
  # Content of the new tab page
  # 
  "browser.newtabpage.enhanced" = false;
  # Disable autoplay of <code>&lt;video&gt;</code> tags.
  # Per default, <code>&lt;video&gt;</code> tags are allowed to start automatically.
  # Note: When disabling autoplay, you will have to click pause and play again on
  # some video sites.
  "media.autoplay.enabled" = true;
  "media.autoplay.default" = 0;

  # Browser Features

  # Disable Telemetry
  # The telemetry feature
  # (https://support.mozilla.org/kb/share-telemetry-data-mozilla-help-improve-firefox)
  # sends data about the performance and responsiveness of Firefox to Mozilla.
  "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.rejected" = true;
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.unifiedIsOptIn" = false;
  "toolkit.telemetry.prompted" = 2;
  "toolkit.telemetry.server" = "";
  "toolkit.telemetry.cachedClientID" = "";
  "toolkit.telemetry.newProfilePing.enabled" = false;
  "toolkit.telemetry.shutdownPingSender.enabled" = false;
  "toolkit.telemetry.updatePing.enabled" = false;
  "toolkit.telemetry.bhrPing.enabled" = false;
  "toolkit.telemetry.firstShutdownPing.enabled" = false;
  "toolkit.telemetry.hybridContent.enabled" = false;
  "toolkit.telemetry.reportingpolicy.firstRun" = false;
  # Disable health report
  # Disable sending Firefox health reports
  # (https://www.mozilla.org/privacy/firefox/#health-report) to Mozilla
  "datareporting.healthreport.uploadEnabled" = false;
  "datareporting.policy.dataSubmissionEnabled" = false;
  "datareporting.healthreport.service.enabled" = false;
  # Disable shield studies
  # Mozilla shield studies (https://wiki.mozilla.org/Firefox/Shield) is a feature
  # which allows mozilla to remotely install experimental addons.
  "app.normandy.enabled" = false;
  "app.normandy.api_url" = "";
  "app.shield.optoutstudies.enabled" = false;
  "extensions.shield-recipe-client.enabled" = false;
  "extensions.shield-recipe-client.api_url" = "";
  # Disable experiments
  # Telemetry Experiments (https://wiki.mozilla.org/Telemetry/Experiments) is a
  # feature that allows Firefox to automatically download and run specially-designed
  # restartless addons based on certain conditions.
  "experiments.enabled" = false;
  "experiments.manifest.uri" = "";
  "experiments.supported" = false;
  "experiments.activeExperiment" = false;
  "network.allow-experiments" = false;
  # Disable Crash Reports
  # The crash report (https://www.mozilla.org/privacy/firefox/#crash-reporter) may
  # contain data that identifies you or is otherwise sensitive to you.
  "breakpad.reportURL" = "";
  "browser.tabs.crashReporting.sendReport" = false;
  "browser.crashReports.unsubmittedCheck.enabled" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
  # Opt out metadata updates
  # Firefox sends data about installed addons as metadata updates
  # (https://blog.mozilla.org/addons/how-to-opt-out-of-add-on-metadata-updates/), so
  # Mozilla is able to recommend you other addons.
  "extensions.getAddons.cache.enabled" = false;
  # Disable google safebrowsing
  # Google safebrowsing can detect phishing and malware but it also sends
  # informations to google together with an unique id called wrkey
  # (http://electroholiker.de/?p=1594).
  "browser.safebrowsing.enabled" = false;
  "browser.safebrowsing.downloads.remote.url" = "";
  "browser.safebrowsing.phishing.enabled" = false;
  "browser.safebrowsing.blockedURIs.enabled" = false;
  "browser.safebrowsing.downloads.enabled" = false;
  "browser.safebrowsing.downloads.remote.enabled" = false;
  "browser.safebrowsing.appRepURL" = "";
  "browser.safebrowsing.malware.enabled" = false;
  # Disable malware scan
  # The malware scan sends an unique identifier for each downloaded file to Google.
  # "browser.safebrowsing.appRepURL" = ""; (Repeated from google safebrowsing)
  # "browser.safebrowsing.malware.enabled" = false; (Repeated from google safebrowsing)
  # Disable DNS over HTTPS
  # DNS over HTTP (DoH), aka. Trusted Recursive Resolver (TRR)
  # (https://wiki.mozilla.org/Trusted_Recursive_Resolver), uses a server run by
  # Cloudflare to resolve hostnames, even when the system uses another (normal) DNS
  # server. This setting disables it and sets the mode to explicit opt-out (5).
  "network.trr.mode" = 5;
  # Disable preloading of the new tab page.
  # By default Firefox preloads the new tab page (with website thumbnails) in the
  # background before it is even opened.
  "browser.newtab.preload" = false;
  # Disable about:addons' Get Add-ons panel
  # The start page with recommended addons uses google analytics.
  "extensions.getAddons.showPane" = false;
  "extensions.webservice.discoverURL" = "";
  # Disable check for captive portal.
  # By default, Firefox checks for the presence of a captive portal on every
  # startup.  This involves traffic to Akamai
  # (https://support.mozilla.org/questions/1169302).
  "network.captive-portal-service.enabled" = false;
  # Disables playback of DRM-controlled HTML5 content
  # if enabled, automatically downloads the Widevine Content Decryption Module
  # provided by Google Inc. Details
  # (https://support.mozilla.org/en-US/kb/enable-drm#w_opt-out-of-cdm-playback-uninstall-cdms-and-stop-all-cdm-downloads)
  "media.eme.enabled" = false;
  # Disables the Widevine Content Decryption Module provided by Google Inc.
  # Used for the playback of DRM-controlled HTML5 content Details
  # (https://support.mozilla.org/en-US/kb/enable-drm#w_disable-the-google-widevine-cdm-without-uninstalling)
  "media.gmp-widevinecdm.enabled" = false;
  # Disable access to device sensor data
  # Disallow websites to access sensor data (ambient light, motion, device
  # orientation and proximity data).
  "device.sensors.ambientLight.enabled" = false;
  "device.sensors.enabled" = false;
  "device.sensors.motion.enabled" = false;
  "device.sensors.orientation.enabled" = false;
  "device.sensors.proximity.enabled" = false;
  # Disable Firefox Suggest
  # The Firefox Suggest
  # (https://support.mozilla.org/en-US/kb/navigate-web-faster-firefox-suggest)
  # feature allows Mozilla to provide search suggestions in the US, which uses your
  # city location and search keywords to send suggestions. This is also used to
  # serve advertisements.
  "browser.urlbar.groupLabels.enabled" = false;
  "browser.urlbar.quicksuggest.enabled" = false;
  # Disable Javascript in PDF viewer
  # Disables executing of JavaScript in the PDF form viewer. It is possible that
  # some PDFs are not rendered correctly due to missing functions.
  "pdfjs.enableScripting" = true;

  # Privacy

  # Fake another Useragent
  # Using a popular useragent string
  # (https://techblog.willshouse.com/2012/01/03/most-common-user-agents/) avoids
  # attracting attention i.e. with an Iceweasel UA. (keep blank to use the default)

  # Block Cookies
  # Block 3rd-Party cookies or even all cookies.
  "network.cookie.cookieBehavior" = 1;
  # Block Referer
  # Firefox tells a website, from which site you're coming (the so called RefControl
  # (http://kb.mozillazine.org/Network.http.sendRefererHeader">referer</a>). You can
  # find more detailed settings in this <a
  # href="http://www.ghacks.net/2015/01/22/improve-online-privacy-by-controlling-referrer-information/">ghacks
  # article</a> or install the <a
  # href="https://addons.mozilla.org/firefox/addon/refcontrol/) extension for per
  # domain settings.
  "network.http.referer.spoofSource" = true;
  # Disable DOM storage
  # Disables DOM storage, which enables so called "supercookies". Some modern sites
  # will not work (i.e. missing "save" functions).
  "dom.storage.enabled" = true;
  # Disable IndexedDB (breaks things)
  # abused for tracking (http://www.w3.org/TR/IndexedDB/">IndexedDB</a> is a way,
  # websites can store structured data. This can be <a
  # href="http://arstechnica.com/apple/2010/09/rldguid-tracking-cookies-in-safari-database-form/),
  # too. Disabling causes problems when sites depend on it like Tweetdeck or Reddit
  # and extensions that use it to store their data. Some users reported crashing
  # tabs when IndexedDB is disabled. Only disable it, when you know what you're
  # doing.
  "dom.indexedDB.enabled" = true;
  # Disable the Offline Cache.
  # Websites can store up to 500 MB of data in an offline cache
  # (http://kb.mozillazine.org/Browser.cache.offline.enable), to be able to run even
  # when there is no working internet connection. This could possibly be used to
  # store an user id.
  "browser.cache.offline.enable" = false;
  # Sessionstore Privacy
  # This preference controls when to store extra information about a session:
  # contents of forms, scrollbar positions, cookies, and POST data.
  "browser.sessionstore.privacy_level" = 2;
  # Disable Link Prefetching
  # Firefox prefetches the next site on some links, so the site is loaded even when
  # you never click.
  "network.prefetch-next" = false;
  "network.dns.disablePrefetch" = true;
  "network.dns.disablePrefetchFromHTTPS" = true;
  "network.predictor.enabled" = false;
  "network.predictor.enable-prefetch" = false;
  # Disable speculative website loading.
  # In some situations Firefox already starts loading web pages when the mouse
  # pointer is over a link, i. e. before you actually click. This is to speed up the
  # loading of web pages by a few milliseconds.
  "network.http.speculative-parallel-limit" = 0;
  "browser.urlbar.speculativeConnect.enabled" = false;
  # Use a private container for new tab page thumbnails
  # Load the pages displayed on the new tab page in a private container when
  # creating thumbnails.
  "privacy.usercontext.about_newtab_segregation.enabled" = true;
  # Disable WebGL
  # Disables the WebGL function, to prevent (ab)use the full power of the graphics
  # card (http://www.uniquemachine.org/">fingerprinting with WebGL</a>. Another
  # issue is, that websites can <a
  # href="https://isc.sans.edu/forums/diary/Time+to+disable+WebGL/10867). WebGL is
  # part of some fingerprinting scripts used in the wild. Some interactive websites
  # will not work, which are mostly games.
  "webgl.disabled" = true;
  # Override graphics card vendor and model strings in the WebGL API
  # Websites can read the graphics card vendor and model using a WebGL API. This
  # setting overrides both with " " without disabling WebGL.
  "webgl.renderer-string-override" = " ";
  "webgl.vendor-string-override" = " ";
  # Disable WebRTC
  # Disables the WebRTC function, which gives away your local ips. Some addons like
  # uBlock origin provide settings to prevent WebRTC from exposing local ips without
  # disabling WebRTC.
  "media.peerconnection.enabled" = false;
  # Disable the clipboardevents.
  # Disable that websites can get notifications if you copy, paste, or cut something
  # from a web page, and it lets them know which part of the page had been selected.
  "dom.event.clipboardevents.enabled" = false;
  # Disable Search Suggestions
  # Firefox suggests search terms in the search field. This will send everything
  # typed or pasted in the search field to the chosen search engine, even when you
  # did not press enter.
  "browser.search.suggest.enabled" = true;
  # Disable Search Keyword
  # When you mistype some url, Firefox starts a search even from urlbar. This
  # feature is useful for quick searching, but may harm your privacy, when it's
  # unintended.
  "keyword.enabled" = true;
  # Disable Fixup URLs
  # When you type "something" in the urlbar and press enter, Firefox tries
  # "something.com", if Fixup URLs is enabled.
  "browser.fixup.alternate.enabled" = true;

  # Website Tracking

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
  "browser.send_pings" = false;
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

  # Security

  # Disable automatic updates.
  # Updates are no longer installed automatically. You will still be notified when
  # an update is available and can install it. Avoids getting a new (maybe addon
  # incompatible) version.
  "app.update.auto" = false;

  # Disable searching for updates.
  # Disable searching for updates. This only works with the enterprise policy
  # download..

  # Disable extension blocklist from mozilla.
  # The extension blocklist (https://blocked.cdn.mozilla.net/) is used by mozilla to
  # deactivate individual addons in the browser, but as a side effect it gives
  # mozilla the ultimate control to disable any extension. <b>Caution:</b> When you
  # disable the blocklist, you may keep using known malware addons.
  "extensions.blocklist.enabled" = false;
  # Enable HTTPS only mode
  # If enabled, allows connections only to sites that use the HTTPS protocol.
  "dom.security.https_only_mode" = true;
  "dom.security.https_only_mode_ever_enabled" = true;
  # Show Punycode.
  # This helps to protect against possible character spoofing.
  "network.IDN_show_punycode" = true;

}
