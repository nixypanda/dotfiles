#!/home/sherub/.nix-profile/bin/python3

# Taken from https://github.com/haideralipunjabi/polybar-browsermediacontrol
import argparse
import json
import os
from os import path

import requests
from gi.repository import GLib
from pydbus import SessionBus

ALBUM_ART_TMP_LOC = "/tmp/bmc-album-art.jpg"
ALBUM_ART_META_TMP_LOC = "/tmp/bmc-album-art.json"
DEFAULT_ALBUM_ART_LOC = "~/.dotfiles/images/music.png"

ICON_PLAY = ""
ICON_PAUSE = ""
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


def album_art_is_up_to_date(url: str) -> bool:
    if not path.exists(ALBUM_ART_TMP_LOC):
        return False
    if not path.exists(ALBUM_ART_META_TMP_LOC):
        return False

    try:
        with open(ALBUM_ART_META_TMP_LOC, "r") as f:
            furl = json.load(f)["url"]
    except Exception:
        return False
    else:
        return furl == url


def download_album_art(url: str):
    res = requests.get(url)
    if res.status_code == 200:
        with open(ALBUM_ART_TMP_LOC, "wb") as f:
            f.write(res.content)
        with open(ALBUM_ART_META_TMP_LOC, "w") as f:
            f.write(json.dumps({"url": url}))


title = Player.Metadata["xesam:title"]

if args.display == "song":
    print(Player.Metadata["xesam:title"])
    exit()
if args.display == "artist":
    print(Player.Metadata["xesam:artist"])
    exit()
if args.display == "cover":
    url = Player.Metadata["mpris:artUrl"]
    if album_art_is_up_to_date(url):

        print(ALBUM_ART_TMP_LOC)
    else:
        try:
            download_album_art(url)
        except Exception as e:
            print(DEFAULT_ALBUM_ART_LOC)
        else:
            print(ALBUM_ART_TMP_LOC)
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


# Polybar has some issues with how it displays stuff
ICON += " "

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
