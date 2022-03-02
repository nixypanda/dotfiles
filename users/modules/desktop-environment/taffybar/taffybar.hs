{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import GI.Gtk
  ( Orientation (OrientationHorizontal),
    Widget,
    boxNew,
    buttonNew,
    buttonNewWithLabel,
    containerAdd,
    onButtonClicked,
    toWidget,
    widgetShow,
  )
import System.Process (spawnCommand)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (TaffyIO, TaffybarConfig)
import System.Taffybar.Hooks (withLogServer, withToggleServer)
import System.Taffybar.SimpleConfig
  ( Position (Top),
    SimpleTaffyConfig (..),
    defaultSimpleTaffyConfig,
    toTaffyConfig,
  )
import System.Taffybar.Util (runCommand)
import System.Taffybar.Widget
  ( LayoutConfig (LayoutConfig, formatLayout),
    Workspace (windows, workspaceName, workspaceState),
    WorkspaceState (..),
    WorkspacesConfig (labelSetter, minIcons, showWorkspaceFn, widgetGap),
    buildContentsBox,
    commandRunnerNew,
    defaultClockConfig,
    defaultWindowsConfig,
    defaultWorkspacesConfig,
    layoutNew,
    textClockNewWith,
    widgetSetClassGI,
    windowsNew,
    workspacesNew,
  )
import System.Taffybar.Widget.SNITray (sniTrayNew)
import System.Taffybar.Widget.SimpleClock (ClockConfig (clockFormatString))

main :: IO ()
main = startTaffybar exampleTaffybarConfig

exampleTaffybarConfig :: TaffybarConfig
exampleTaffybarConfig =
  withLogServer $ withToggleServer $ toTaffyConfig myConfig

myConfig :: SimpleTaffyConfig
myConfig =
  defaultSimpleTaffyConfig
    { startWidgets = workspaces : map (>>= buildContentsBox) [layout, musicPlayer],
      centerWidgets = map (>>= buildContentsBox) [windowsW],
      endWidgets = powerMenu : map (>>= buildContentsBox) [clock, tray],
      barPosition = Top,
      barHeight = 50,
      widgetSpacing = 10,
      cssPath = Just "/home/sherub/.dotfiles/users/modules/desktop-environment/taffybar/taffybar.css"
    }

myWorkspacesConfig :: WorkspacesConfig
myWorkspacesConfig =
  let showLabelOnlyForEmptyWorkspace ws
        | workspaceState ws == Empty = workspaceName ws
        | workspaceState ws == Active && null (windows ws) = workspaceName ws
        | otherwise = ""
   in defaultWorkspacesConfig
        { labelSetter = return . showLabelOnlyForEmptyWorkspace
        }

workspaces :: TaffyIO Widget
workspaces = workspacesNew myWorkspacesConfig

clock :: TaffyIO Widget
clock = textClockNewWith $ defaultClockConfig {clockFormatString = "  %a %_d %b   %I:%M %p  "}

myLayoutConfig :: LayoutConfig
myLayoutConfig =
  let layoutDisplay "Spacing Tall" = "█▌▋"
      layoutDisplay "Spacing ThreeCol" = "█▐▐"
      layoutDisplay "Spacing Full" = "███"
      layoutDisplay "Spacing Mirror Tall" = "▀▀▀"
      layoutDisplay x = x
   in LayoutConfig
        { formatLayout = return . layoutDisplay
        }

layout :: TaffyIO Widget
layout = layoutNew myLayoutConfig

windowsW :: TaffyIO Widget
windowsW = windowsNew defaultWindowsConfig

tray :: TaffyIO Widget
tray = sniTrayNew

powerMenu :: TaffyIO Widget
powerMenu = commandButton "power-menu" "\61457" powerMenuLauncher
  where
    powerMenuLauncher =
      T.unwords
        [ "eww",
          "open-many",
          "--toggle",
          "topbar-powermenu-bg",
          "topbar-powermenu-logout",
          "topbar-powermenu-sleep",
          "topbar-powermenu-reboot",
          "topbar-powermenu-poweroff"
        ]

musicPlayer :: TaffyIO Widget
musicPlayer = do
  box <- boxNew OrientationHorizontal 5

  icon <- commandRunnerNew 1 "custom-browsermediacontrol" ["--display", "status-icon"] "⏯"
  buttonPlay <- buttonNew
  _ <- containerAdd buttonPlay icon
  _ <- widgetSetClassGI buttonPlay "music-player-play-btn"

  buttonPrev <- buttonNewWithLabel "玲"
  buttonNext <- buttonNewWithLabel "怜  "

  song <- commandRunnerNew 1 "custom-browsermediacontrol" ["--display", "song"] "Failed to get name"
  songButton <- buttonNew
  _ <- containerAdd songButton song

  _ <- onButtonClicked buttonPrev $ void (spawnCommand (T.unpack "custom-browsermediacontrol --prev"))
  _ <- onButtonClicked buttonPlay $ void (spawnCommand (T.unpack "custom-browsermediacontrol --playpause"))
  _ <- onButtonClicked buttonNext $ void (spawnCommand (T.unpack "custom-browsermediacontrol --next"))
  _ <- onButtonClicked songButton $ void (spawnCommand (T.unpack "eww open-many --toggle topbar-music-bg topbar-music"))

  _ <- containerAdd box buttonPrev
  _ <- containerAdd box buttonPlay
  _ <- containerAdd box buttonNext
  _ <- containerAdd box songButton

  _ <- widgetSetClassGI box "music-player"
  toWidget box

commandButton :: T.Text -> T.Text -> T.Text -> TaffyIO Widget
commandButton className label cmd = do
  button <- buttonNewWithLabel label
  _ <- onButtonClicked button $ void (spawnCommand (T.unpack cmd))
  _ <- widgetSetClassGI button className
  widgetShow button
  toWidget button
