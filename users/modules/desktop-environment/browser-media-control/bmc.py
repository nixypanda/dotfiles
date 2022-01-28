#!/home/sherub/.nix-profile/bin/python3

# Taken from https://github.com/haideralipunjabi/polybar-browsermediacontrol
import argparse
import json
import os
from dataclasses import dataclass
from os import path
from typing import Optional

import requests
from gi.repository import GLib
from pydbus import SessionBus

ALBUM_ART_TMP_LOC = "/tmp/bmc-album-art.jpg"
ALBUM_ART_META_TMP_LOC = "/tmp/bmc-album-art.json"
DEFAULT_ALBUM_ART_LOC = path.expanduser("~/.dotfiles/images/music.png")

ICON_PLAY = ""
ICON_PAUSE = ""
ICON_STOPPED = "■"
ICON_NEXT = "怜"
ICON_PREV = "玲"
ICON = ""
PATH = os.path.realpath(__file__)
TITLE_LENGTH = 25


@dataclass
class Song:
    title: str
    artist: str
    art_url: Optional[str]
    length: int


no_song = Song(
    title="No song playing",
    artist="None",
    art_url=None,
    length=0,
)


class Player:
    def __init__(self, bus):
        try:
            self.player = bus.get(
                "org.kde.plasma.browser_integration", "/org/mpris/MediaPlayer2"
            )
        except GLib.Error:
            self.player = None

    @property
    def volume(self) -> float:
        if not self.player:
            return 0
        return self.player.Volume

    def inc_volume(self, increment: float):
        if not self.player:
            return
        self.player.Volume = self.volume + increment * 0.1

    def playpause(self):
        if not self.player:
            return
        self.player.PlayPause()

    def next(self):
        if not self.player:
            return
        self.player.Next()

    def prev(self):
        if not self.player:
            return
        self.player.Previous()

    @property
    def percent_played(self) -> float:
        if not self.player:
            return 0
        if not self.current_song.length:
            return 0

        return as_percent(self.player.Position, self.current_song.length)

    @property
    def status(self):
        if not self.player:
            return "Stopped"
        return self.player.PlaybackStatus

    @property
    def current_song(self):
        if not self.player:
            return no_song

        song = self.player.Metadata
        try:
            return Song(
                title=song["xesam:title"],
                artist=song["xesam:artist"],
                art_url=song["mpris:artUrl"],
                length=song["mpris:length"],
            )
        except KeyError:
            return no_song


def as_percent(value, total):
    if total == 0:
        return 0
    return int(value / total * 100)


def album_art_is_up_to_date(url: Optional[str]) -> bool:
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


def status_to_icon(status: str) -> str:
    if status == "Playing":
        return ICON_PAUSE
    elif status == "Paused":
        return ICON_PLAY
    else:
        return ICON_STOPPED


def truncate(text, max_len):
    if len(text) > max_len:
        return f"{text[:max_len]}..."
    return text


def display(player: Player, arg):
    song = player.current_song

    if arg == "song":
        return song.title
    if arg == "artist":
        return song.artist
    if arg == "cover":
        if song.art_url is None:
            return DEFAULT_ALBUM_ART_LOC
        elif album_art_is_up_to_date(song.art_url):
            return ALBUM_ART_TMP_LOC
        else:
            try:
                download_album_art(song.art_url)
            except Exception:
                return DEFAULT_ALBUM_ART_LOC
            else:
                return ALBUM_ART_TMP_LOC
    if arg == "position":
        return player.percent_played
    if arg == "status":
        return player.status
    if arg == "status-icon":
        return status_to_icon(player.status)
    else:
        raise ValueError("Did not expect to reach here")


def polybar_display(player: Player, arg):
    def action(command, text):
        # %{A1:PATH -- prev :}I%{A}
        return "%{A1:" + PATH + " --" + command + ":}" + text + "%{A}"

    song = player.current_song
    icon = status_to_icon(player.status) + " "

    prev_icon = action("prev", ICON_PREV)
    pp_icon = action("playpause", icon)
    next_icon = action("next", ICON_NEXT)
    title = truncate(song.title, TITLE_LENGTH)

    if arg == "full":
        return f"{prev_icon} {pp_icon} {next_icon} {title}"
    if arg == "play/pause":
        return pp_icon
    if arg == "title":
        return title
    if arg == "next":
        return next_icon
    else:
        raise ValueError("Did not expect to reach here")


def main():
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
    player = Player(SessionBus())

    if args.volume is not None:
        player.inc_volume(args.volume)
    elif args.playpause:
        player.playpause()
    elif args.next:
        player.next()
    elif args.prev:
        player.prev()
    elif args.display is not None:
        result = display(player, args.display)
        print(result)
    elif args.polybar_display is not None:
        result = polybar_display(player, args.polybar_display)
        print(result)
    else:
        raise ValueError("Did not expect to reach here")


if __name__ == "__main__":
    main()
