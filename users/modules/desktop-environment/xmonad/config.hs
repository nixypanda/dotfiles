import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioRaiseVolume,
  )
import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.InsertPosition (Focus (Newer), Position (End), insertPosition)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Layout.Accordion (Accordion (Accordion))
import XMonad.Layout.Gaps
  ( Direction2D (D, L, R, U),
    GapMessage (ModifyGaps, ToggleGaps),
    GapSpec,
    Gaps,
    gaps,
  )
import XMonad.Layout.Grid (Grid (Grid, GridRatio))
import XMonad.Layout.LayoutModifier (ModifiedLayout (ModifiedLayout))
import XMonad.Layout.MultiToggle
  ( EOT,
    HCons,
    MultiToggle,
    Toggle (Toggle),
    mkToggle,
    single,
  )
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import XMonad.Layout.NoBorders
  ( Ambiguity (OnlyScreenFloat),
    ConfigurableBorder,
    lessBorders,
  )
import XMonad.Layout.PerScreen (PerScreen, ifWider)
import XMonad.Layout.Spacing
  ( Border (Border),
    Spacing (Spacing),
    bottom,
    decWindowSpacing,
    incWindowSpacing,
    left,
    right,
    screenBorder,
    screenBorderEnabled,
    smartBorder,
    toggleWindowSpacingEnabled,
    top,
    windowBorder,
    windowBorderEnabled,
  )
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol, threeColDelta, threeColFrac, threeColNMaster))
import XMonad.Layout.TwoPanePersistent (TwoPanePersistent (TwoPanePersistent, dFrac, mFrac, slaveWin))
import XMonad.StackSet
  ( RationalRect (RationalRect),
    current,
    focusMaster,
    greedyView,
    layout,
    shift,
    workspace,
  )
import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    cmd,
    customFloating,
    hook,
    name,
    namedScratchpadAction,
    namedScratchpadManageHook,
    query,
  )

main :: IO ()
main = do
  xmonad myConfig

myConfig :: XConfig (MyLayoutModifiers MyTogglableLayouts)
myConfig =
  docks . ewmh . ewmhFullscreen . pagerHints $
    def
      { terminal = myTerminal,
        startupHook = myStartupHook,
        manageHook = myManageHook,
        layoutHook = myLayoutModifiers myTogglableLayouts,
        handleEventHook = handleEventHook def,
        -- NOTE: Injected using nix strings.
        -- Think about parsing colorscheme.nix file in some way
        focusedBorderColor = myFocusedBorderColor,
        normalBorderColor = myNormalBorderColor,
        borderWidth = 3
      }
      -- NOTE: Ordering matters here
      `additionalKeys` keysToAdd

myModMask :: KeyMask
myModMask = mod1Mask

myTerminal :: String
myTerminal = "kitty"

openWidgetsPanel :: String
openWidgetsPanel =
  unwords
    [ "eww",
      "open-many",
      "--toggle",
      "full-bg",
      "full-profile",
      "full-system",
      "full-clock",
      "full-uptime",
      "full-music",
      "full-github",
      "full-reddit",
      "full-twitter",
      "full-youtube",
      "full-weather",
      "full-apps",
      "full-mail",
      "full-nix_search",
      "full-goodreads",
      "full-logout",
      "full-sleep",
      "full-reboot",
      "full-poweroff",
      "full-folders"
    ]

openPowerMenu :: String
openPowerMenu =
  unwords
    [ "eww",
      "open-many",
      "--toggle",
      "powermenu-bg",
      "powermenu-logout",
      "powermenu-sleep",
      "powermenu-reboot",
      "powermenu-poweroff"
    ]

keysToAdd :: [((KeyMask, KeySym), X ())]
keysToAdd =
  launchers
    ++ multimediaKeys
    ++ layoutRelated
  where
    launchers =
      [ ( (myModMask, xK_Return),
          spawn "rofi -show drun -theme grid"
        ),
        ( (myModMask .|. controlMask, xK_p),
          spawn openPowerMenu
        ),
        ( (myModMask .|. shiftMask, xK_l),
          spawn "betterlockscreen --wall --blur -l"
        ),
        ( (myModMask .|. shiftMask, xK_space),
          spawn $ myTerminal ++ " -d \"$(xcwd)\""
        ),
        ( (myModMask .|. shiftMask, xK_4),
          spawn "sleep 0.2 && scrot -s ~/Pictures/screenshots/scrot_%Y-%m-%d-%H%M%S.png"
        ),
        ( (myModMask .|. shiftMask, xK_5),
          spawn "sleep 0.2 && scrot -d 5 ~/Pictures/screenshots/scrot_%Y-%m-%d-%H%M%S.png"
        ),
        ( (myModMask, xK_c),
          namedScratchpadAction myScratchPads "terminal"
        ),
        ( (myModMask .|. shiftMask, xK_n),
          spawn "kill -s USR1 $(pidof deadd-notification-center)"
        ),
        ( (myModMask .|. shiftMask, xK_w),
          spawn openWidgetsPanel
        )
      ]

    multimediaKeys =
      [ ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 5%-"),
        ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 5%+"),
        ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle")
      ]
    layoutRelated =
      [ ((myModMask, xK_n), sendMessage NextLayout),
        ((myModMask, xK_m), sendMessage $ Toggle FULL),
        ((myModMask .|. shiftMask, xK_m), windows focusMaster),
        ((myModMask .|. controlMask, xK_g), sendMessage ToggleGaps),
        ((myModMask .|. controlMask, xK_h), sendMessage $ ModifyGaps incGap),
        ((myModMask .|. controlMask, xK_f), sendMessage $ ModifyGaps decGap),
        ((myModMask .|. controlMask, xK_s), toggleWindowSpacingEnabled),
        ((myModMask .|. controlMask, xK_d), incWindowSpacing 5),
        ((myModMask .|. controlMask, xK_a), decWindowSpacing 5)
      ]
      where
        decGap :: GapSpec -> GapSpec
        decGap = fmap (fmap (subtract 5))

        incGap :: GapSpec -> GapSpec
        incGap = fmap (fmap (+ 5))

type MyLayouts = PerScreen HorizontalMonitorLayouts VerticalMonitorLayouts

type HorizontalMonitorLayouts = (Choose Tall (Choose TwoPanePersistent (Choose ThreeCol Grid)))

type VerticalMonitorLayouts = (Choose (Mirror Tall) (Choose Accordion Grid))

myLayouts :: MyLayouts Window
myLayouts = ifWider minHorizontalWidth horizontalMonitorLayouts verticalMonitorLayouts
  where
    horizontalMonitorLayouts = tall ||| twoPane ||| threeCol ||| Grid
    verticalMonitorLayouts = Mirror tall ||| Accordion ||| invertedGrid
    -- we are using the `ifWider` layoutModifier to have two different sets of
    -- layout modifiers for monitors in horizontal or vertical configuration
    minHorizontalWidth = 2560

    tall = Tall {tallNMaster = 1, tallRatioIncrement = 3 / 100, tallRatio = 3 / 5}
    twoPane = TwoPanePersistent {slaveWin = Nothing, dFrac = 3 / 100, mFrac = 3 / 5}
    threeCol = ThreeCol {threeColNMaster = 1, threeColDelta = 3 / 100, threeColFrac = 1 / 2}
    invertedGrid = GridRatio (9 / 16)

type MyTogglableLayouts = MultiToggle (HCons StdTransformers EOT) MyLayouts

myTogglableLayouts :: MyTogglableLayouts Window
myTogglableLayouts = mkToggle (single FULL) myLayouts

type MyLayoutModifiers a =
  ModifiedLayout
    (ConfigurableBorder Ambiguity)
    (ModifiedLayout AvoidStruts (ModifiedLayout Spacing (ModifiedLayout Gaps a)))

myLayoutModifiers ::
  MyTogglableLayouts Window ->
  (MyLayoutModifiers MyTogglableLayouts) Window
myLayoutModifiers =
  lessBorders OnlyScreenFloat . avoidStruts . spacingLayoutSetup . gapLayoutSetup
  where
    spacingLayoutSetup :: l a -> ModifiedLayout Spacing l a
    spacingLayoutSetup =
      ModifiedLayout $
        Spacing
          { smartBorder = False,
            screenBorder = screenBorder,
            screenBorderEnabled = False,
            windowBorder = windowBorder,
            windowBorderEnabled = True
          }

    gapLayoutSetup :: l a -> ModifiedLayout Gaps l a
    gapLayoutSetup =
      gaps [(U, edgeGap), (R, edgeGap), (D, edgeGap), (L, edgeGap)]

    screenBorder = Border {top = 5, bottom = 5, right = 5, left = 5}
    windowBorder = Border {top = 5, bottom = 5, right = 5, left = 5}
    edgeGap = 4

myScratchPads :: [NamedScratchpad]
myScratchPads = [scTerminal]
  where
    scTerminal =
      NS
        { name = "terminal",
          cmd = myTerminal ++ " --class=scratchpad",
          query = className =? "scratchpad",
          hook = customFloating largeRect
        }
    largeRect = RationalRect l t w h
      where
        h = 2 / 3
        w = 2 / 3
        t = 1 / 6
        l = 1 / 6

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ manageDocks,
      className =? "OpenRGB" --> doFloat,
      className =? "Lxappearance" --> doFloat,
      className =? ".solaar-wrapped" --> doFloat,
      className =? "Psensor" --> doFloat,
      className =? "Nm-connection-editor" --> doFloat,
      className =? "Gddccontrol" --> doFloat,
      className =? "feh" --> doFullFloat,
      className =? ".blueman-manager-wrapped" --> doFloat,
      className =? "Pavucontrol" --> doFloat,
      namedScratchpadManageHook myScratchPads,
      insertPosition End Newer
    ]

myStartupHook :: X ()
myStartupHook = do
  spawn "custom-panel-launch"
