#!/home/sherub/.nix-profile/bin/python3

# Taken from https://github.com/haideralipunjabi/polybar-browsermediacontrol
import argparse
import os

from gi.repository import GLib
from pydbus import SessionBus

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
parser.add_argument(
    "--display",
    type=str,
    choices=["song", "artist", "cover", "position", "status", "status-icon"],
)
parser.add_argument(
    "--polybar-display", type=str, choices=["play/pause", "title", "next", "full"]
)
args = parser.parse_args()

bus = SessionBus()


try:
    Player = bus.get("org.kde.plasma.browser_integration", "/org/mpris/MediaPlayer2")
except GLib.Error:
    exit()

# Music control

# TODO: Handle this case
if Player.PlaybackStatus == "Stopped":
    print("")
    exit()

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


# Info Display


def as_percent(value, total):
    return int(value / total * 100)


title = Player.Metadata["xesam:title"]

if args.display == "song":
    print(Player.Metadata["xesam:title"])
    exit()
if args.display == "artist":
    print(Player.Metadata["xesam:artist"])
    exit()
if args.display == "cover":
    print(Player.Metadata["mpris:artUrl"])
    exit()
if args.display == "position":
    print(as_percent(Player.Position, Player.Metadata["mpris:length"]))
    exit()
if args.display == "status":
    print(Player.PlaybackStatus)
    exit()
if args.display == "status-icon":
    print(ICON)
    exit()


# Polybar display


def action(command, text):
    # %{A1:PATH -- prev :}I%{A}
    return "%{A1:" + PATH + " --" + command + ":}" + text + "%{A}"


def truncate(text, max_len):
    if len(text) > max_len:
        return f"{text[:max_len]}..."
    return text


if args.polybar_display == "full":
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
if args.polybar_display == "play/pause":
    print(action("playpause", ICON))
    exit()
if args.polybar_display == "title":
    print(truncate(title, TITLE_LENGTH))
    exit()
if args.polybar_display == "next":
    print(action("next", ICON_NEXT))
    exit()

exit()
