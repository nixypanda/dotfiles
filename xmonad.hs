import XMonad

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhDesktopsLogHook, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)

import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Gaps (Gaps, Direction2D(..), gaps)
import XMonad.Layout.Spacing (Spacing, Border(..), spacingRaw)
import XMonad.Layout.NoBorders (noBorders)

import XMonad.Util.EZConfig (additionalKeys, removeKeys)

import XMonad.StackSet (greedyView, shift)
import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_AudioLowerVolume
    , xF86XK_AudioRaiseVolume
    , xF86XK_AudioMute
    )


main :: IO ()
main = do
    xmonad $ docks $ ewmh myConfig


myConfig =
    let
        myWorkspaces :: [String]
        myWorkspaces = ["a", "s", "d", "f"]

        modMask :: KeyMask
        modMask = mod1Mask

        tall :: Tall a
        tall = Tall 1 (3/100) (1/2)

        keysToRemove :: [(KeyMask, KeySym)]
        keysToRemove = defaultWorkSpaceSwitchBinding ++ defaultWorkSpaceWindowMoveBinding
            where
                defaultWorkSpaceWindowMoveBinding =
                    [(modMask .|. shiftMask, n) | n <- [xK_1 .. xK_9]] 
                defaultWorkSpaceSwitchBinding =
                    [(modMask, n) | n <- [xK_1 .. xK_9]] 

        keysToAdd :: [((KeyMask, KeySym), X ())]
        keysToAdd = launchers ++ multimediaKeys ++ workspaceSetup ++ layoutRelated
            where
                launchers =
                    [ ( (modMask, xK_space)
                        , spawn "rofi -show drun"
                      )
                    , ( (modMask .|. shiftMask, xK_l)
                        , spawn "i3lock-fancy -p -t \"\""
                      )
                    , ( (modMask .|. shiftMask, xK_Return)
                        , spawn "xterm -e \"cd $(xcwd) && ~/.nix-profile/bin/zsh\""
                      )
                    , ( (modMask .|. shiftMask, xK_4)
                        , spawn "sleep 0.2 && scrot -s ~/Pictures/screenshots/scrot_%Y-%m-%d-%H%M%S.png"
                      )
                    ]
                workspaceSetup = 
                    [ ((m .|. modMask, k), windows $ f i)
                    | (i, k) <- zip (myWorkspaces) [xK_a, xK_s, xK_d, xK_f]
                    , (f, m) <- [(greedyView, 0), (shift, shiftMask)]]
                multimediaKeys =
                    [ ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 5%-")
                    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 5%+")
                    , ((0, xF86XK_AudioMute       ), spawn "amixer sset Master toggle")
                    ]
                layoutRelated =
                    [ ((modMask, xK_n), sendMessage NextLayout)
                    ]

        spacingLayoutSetup :: l a -> ModifiedLayout Spacing l a
        spacingLayoutSetup = spacingRaw False screenBorder True windowBorder True
            where
                screenBorder = Border 5 5 5 5
                windowBorder = Border 5 5 5 5

        gapLayoutSetup :: l a -> ModifiedLayout Gaps l a
        gapLayoutSetup = gaps [ (U, edgeGap), (R, edgeGap), (D, edgeGap), (L, edgeGap) ]
            where edgeGap = 20


        layoutModifiers = avoidStruts . noBorders . spacingLayoutSetup . gapLayoutSetup

        layouts :: Choose Tall Full a
        layouts = tall ||| Full

        myManageHook :: ManageHook
        myManageHook = composeAll
            [ manageDocks
            ]
     in
     def
         { logHook = ewmhDesktopsLogHook
         , terminal = "xterm"
         , startupHook = myStartupHook
         , manageHook =  myManageHook
         , layoutHook = layoutModifiers layouts 
         , workspaces = myWorkspaces
         , handleEventHook = fullscreenEventHook <+> handleEventHook def
         }
         `removeKeys` keysToRemove
         `additionalKeys` keysToAdd


myStartupHook :: X ()
myStartupHook = do
    spawn "custom-i3-polybar-launch"
