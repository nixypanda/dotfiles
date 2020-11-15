#!/home/sherub/.nix-profile/bin/python3

# Taken from https://github.com/haideralipunjabi/polybar-browsermediacontrol
from pydbus import SessionBus

from gi.repository import GLib
import os
import argparse

ICON_PLAY = " "
ICON_PAUSE = " "
ICON_NEXT = "怜"
ICON_PREV = "玲"
ICON = ""
PATH = os.path.realpath(__file__)
TITLE_LENGTH = 25

parser = argparse.ArgumentParser()
parser.add_argument("--volume", type=int)
parser.add_argument("--playpause", action="store_true")
parser.add_argument("--next", action="store_true")
parser.add_argument("--prev", action="store_true")
parser.add_argument("--display", type=str, choices=["play/pause", "title", "next"])
args = parser.parse_args()

bus = SessionBus()

try:
    Player = bus.get("org.kde.plasma.browser_integration", "/org/mpris/MediaPlayer2")
except GLib.Error as e:
    exit()


def action(command, text):
    # %{A1:PATH -- prev :}I%{A}
    return "%{A1:" + PATH + " --" + command + ":}" + text + "%{A}"


def truncate(text, max_len):
    if len(text) > max_len:
        return f"{text[:max_len]}..."
    return text


if Player.PlaybackStatus == "Stopped":
    print("")

if args.volume is not None:
    vol = Player.Volume
    Player.Volume = vol + (args.volume * 0.1)
    exit()

if args.playpause:
    Player.PlayPause()
    exit()

if Player.PlaybackStatus == "Playing":
    ICON = ICON_PAUSE
elif Player.PlaybackStatus == "Paused":
    ICON = ICON_PLAY

if args.next:
    Player.Next()
elif args.prev:
    Player.Previous()

title = Player.Metadata["xesam:title"]

if args.display is None:
    output = (
        action("prev", ICON_PREV)
        + " "
        + action("playpause", ICON)
        + " "
        + action("next", ICON_NEXT)
        + " "
        + truncate(title, TITLE_LENGTH)
    )

    print(output)
    exit()

if args.display == "play/pause":
    print(action("playpause", ICON))
    exit()
if args.display == "title":
    print(truncate(title, TITLE_LENGTH))
    exit()
if args.display == "next":
    print(action("next", ICON_NEXT))
    exit()

exit()
