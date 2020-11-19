import XMonad

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhDesktopsLogHook, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.InsertPosition (Position(End), Focus(Newer), insertPosition)

import XMonad.Layout.LayoutModifier (ModifiedLayout(ModifiedLayout))
import XMonad.Layout.Gaps
    ( Gaps
    , GapSpec
    , GapMessage(ToggleGaps, ModifyGaps)
    , Direction2D(U, R, D, L)
    , gaps
    )
import XMonad.Layout.Spacing
    ( Spacing(Spacing)
    , smartBorder
    , screenBorder
    , screenBorderEnabled
    , windowBorder
    , windowBorderEnabled
    , Border(Border)
    , top
    , bottom
    , right
    , left
    , toggleWindowSpacingEnabled
    , incWindowSpacing
    , decWindowSpacing
    )
import XMonad.Layout.NoBorders
    ( Ambiguity(OnlyScreenFloat)
    , ConfigurableBorder
    , SmartBorder
    , lessBorders
    , smartBorders
    )
import XMonad.Layout.ThreeColumns
    ( ThreeCol(ThreeCol)
    , threeColNMaster
    , threeColDelta
    , threeColFrac
    )
import XMonad.Layout.MultiToggle
    ( MultiToggle
    , EOT
    , HCons
    , Toggle(Toggle)
    , mkToggle
    , single
    )
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL))

import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import XMonad.Util.NamedScratchpad
    ( NamedScratchpad(NS)
    , name
    , cmd
    , query
    , hook
    , namedScratchpadManageHook
    , namedScratchpadAction
    , customFloating
    )

import XMonad.StackSet
    ( RationalRect(RationalRect)
    , greedyView
    , focusMaster
    , shift
    , current
    , workspace
    , layout
    )
import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_AudioLowerVolume
    , xF86XK_AudioRaiseVolume
    , xF86XK_AudioMute
    )


main :: IO ()
main = do
    xmonad . docks . ewmh $ myConfig


myConfig :: XConfig (MyLayoutModifiers MyTogglableLayouts)
myConfig =
     def
         { logHook = myLogHook
         , terminal = myTerminal
         , startupHook = myStartupHook
         , manageHook =  myManageHook
         , layoutHook = myLayoutModifiers myTogglableLayouts
         , workspaces = myWorkspaces
         , handleEventHook = fullscreenEventHook <+> handleEventHook def

         -- NOTE: Injected using nix strings.
         -- Think about parsing colorscheme.nix file in some way
         , focusedBorderColor = myFocusedBorderColor
         , normalBorderColor = myNormalBorderColor
         , borderWidth = 3
         }
         -- NOTE: Ordering matters here
         `removeKeys` keysToRemove
         `additionalKeys` keysToAdd


myWorkspaces :: [String]
myWorkspaces = ["a", "s", "d", "f", "u", "i", "o", "p"]


myModMask :: KeyMask
myModMask = mod1Mask


myTerminal :: String
myTerminal = "xterm"


keysToRemove :: [(KeyMask, KeySym)]
keysToRemove = defaultWorkSpaceSwitchBinding ++ defaultWorkSpaceWindowMoveBinding
    where
        defaultWorkSpaceWindowMoveBinding =
            [(myModMask .|. shiftMask, n) | n <- [xK_1 .. xK_9]] 
        defaultWorkSpaceSwitchBinding =
            [(myModMask, n) | n <- [xK_1 .. xK_9]] 


keysToAdd :: [((KeyMask, KeySym), X ())]
keysToAdd =
      launchers
    ++ multimediaKeys
    ++ workspaceSetup
    ++ workspaceWindowMoveSetup
    ++ layoutRelated
    where
        launchers =
            [ ( (myModMask, xK_space)
                , spawn "rofi -show drun -theme grid"
              )
            , ( (myModMask, xK_e)
                , spawn "rofi -modi \"Powermenu:custom-script-sysmenu\" -show Powermenu -theme sysmenu"
              )
            , ( (myModMask .|. shiftMask, xK_l)
                , spawn "i3lock-fancy -t \"\""
              )
            , ( (myModMask .|. shiftMask, xK_Return)
                , spawn $ myTerminal ++ " -e \"cd $(xcwd) && ~/.nix-profile/bin/zsh\""
              )
            , ( (myModMask .|. shiftMask, xK_4)
                , spawn "sleep 0.2 && scrot -s ~/Pictures/screenshots/scrot_%Y-%m-%d-%H%M%S.png"
              )
            , ( (myModMask .|. shiftMask, xK_5)
                , spawn "sleep 0.2 && scrot -d 5 ~/Pictures/screenshots/scrot_%Y-%m-%d-%H%M%S.png"
              )
            , ( (myModMask, xK_c)
                , namedScratchpadAction myScratchPads "terminal"
              )
            ]

        workspaceKeyAndIdentifiers = 
            zip (myWorkspaces) [xK_a, xK_s, xK_d, xK_f, xK_u, xK_i, xK_o, xK_p]

        workspaceSetup = 
            [ ((myModMask, key), windows $ greedyView identifier)
            | (identifier, key) <- workspaceKeyAndIdentifiers]

        workspaceWindowMoveSetup =
            [ ((shiftMask .|. myModMask, key), windows $ shift identifier)
            | (identifier, key) <- workspaceKeyAndIdentifiers]

        multimediaKeys =
            [ ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 5%-")
            , ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 5%+")
            , ((0, xF86XK_AudioMute       ), spawn "amixer sset Master toggle")
            ]
        layoutRelated =
            [ ((myModMask, xK_n), sendMessage NextLayout)
            , ((myModMask, xK_m), sendMessage $ Toggle FULL)

            , ((myModMask .|. shiftMask, xK_m), windows focusMaster)

            , ((myModMask .|. controlMask, xK_g), sendMessage $ ToggleGaps)
            , ((myModMask .|. controlMask, xK_h), sendMessage $ ModifyGaps incGap)
            , ((myModMask .|. controlMask, xK_f), sendMessage $ ModifyGaps decGap)
            , ((myModMask .|. controlMask, xK_s), toggleWindowSpacingEnabled)
            , ((myModMask .|. controlMask, xK_d), incWindowSpacing 5)
            , ((myModMask .|. controlMask, xK_a), decWindowSpacing 5)
            ]
                where
                    decGap :: GapSpec -> GapSpec
                    decGap = fmap (fmap (subtract 5))

                    incGap :: GapSpec -> GapSpec
                    incGap = fmap (fmap (+ 5))


type MyLayouts = Choose Tall ThreeCol
type MyTogglableLayouts = MultiToggle (HCons StdTransformers EOT) MyLayouts


myTogglableLayouts :: MyTogglableLayouts a
myTogglableLayouts = mkToggle (single FULL) myLayouts


myLayouts :: MyLayouts a
myLayouts = tall ||| threeCol
    where
        tall :: Tall a
        tall = Tall { tallNMaster = 1, tallRatioIncrement = 3/100, tallRatio = 1/2 }

        threeCol :: ThreeCol a
        threeCol = ThreeCol
            { threeColNMaster = 1, threeColDelta = 3/100, threeColFrac = 1/2 }


type MyLayoutModifiers a =
    ModifiedLayout
        (ConfigurableBorder Ambiguity)
        (ModifiedLayout
            AvoidStruts
            (ModifiedLayout
                Spacing
                (ModifiedLayout
                    Gaps
                    (ModifiedLayout 
                        SmartBorder
                        a
                    )
                )
            )
        )


myLayoutModifiers :: MyTogglableLayouts Window -> 
    (MyLayoutModifiers MyTogglableLayouts) Window
myLayoutModifiers =
      lessBorders OnlyScreenFloat
    . avoidStruts
    . spacingLayoutSetup
    . gapLayoutSetup
    . smartBorders
        where
            spacingLayoutSetup :: l a -> ModifiedLayout Spacing l a
            spacingLayoutSetup = ModifiedLayout $
                Spacing
                    { smartBorder = False
                    , screenBorder = screenBorder
                    , screenBorderEnabled = False
                    , windowBorder = windowBorder
                    , windowBorderEnabled = True
                    }

            gapLayoutSetup :: l a -> ModifiedLayout Gaps l a
            gapLayoutSetup =
                gaps [ (U, edgeGap), (R, edgeGap), (D, edgeGap), (L, edgeGap) ]

            screenBorder = Border { top = 5, bottom = 5, right = 5, left = 5 }
            windowBorder = Border { top = 5, bottom = 5, right = 5, left = 5 }
            edgeGap = 20


myScratchPads :: [NamedScratchpad]
myScratchPads = [ scTerminal ]
    where
        scTerminal = NS
            { name = "terminal"
            , cmd = myTerminal ++ " -name scratchpad"
            , query = resource =? "scratchpad"
            , hook = customFloating largeRect
            }
        largeRect = RationalRect l t w h
            where
                h = 2/3
                w = 2/3
                t = 1/6
                l = 1/6


myManageHook :: ManageHook
myManageHook = composeAll
    [ manageDocks
    , className =? "OpenRGB" --> doFloat
    , className =? "Lxappearance" --> doFloat
    , className =? ".solaar-wrapped" --> doFloat
    , className =? "Psensor" --> doFloat
    , className =? "Nm-connection-editor" --> doFloat
    , className =? "Gddccontrol" --> doFloat
    , className =? "Sxiv" --> doFullFloat
    , namedScratchpadManageHook myScratchPads
    , insertPosition End Newer
    ]


myStartupHook :: X ()
myStartupHook = do
    spawn "custom-panel-launch"


layoutDisplay :: String -> String
layoutDisplay "Spacing Tall" = "█▌▋"
layoutDisplay "Spacing ThreeCol" = "█▐▐"
layoutDisplay "Spacing Full" = "███"
layoutDisplay x = x


layoutLogger :: X ()
layoutLogger = withWindowSet $ \ws -> do
    let layoutName = layoutDisplay . description . layout . workspace $ current ws
    io $ appendFile ("/home/sherub/.xmonad/xmonad-layout") (layoutName ++ "\n")


myLogHook :: X ()
myLogHook = ewmhDesktopsLogHook >> layoutLogger
