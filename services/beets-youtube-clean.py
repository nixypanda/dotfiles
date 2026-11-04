"""Clean YouTube-specific metadata before Beets imports a track."""

from __future__ import annotations

import os
import re
from typing import Any

from beets.plugins import BeetsPlugin

_TOPIC_RE = re.compile(r"\s+-\s+topic\s*$", re.IGNORECASE)

_NOISE_RE = re.compile(
    r"""
    \s*
    [\(\[\{]
    \s*
    (?:
        official\s+(?:music\s+)?video
        | official\s+audio
        | lyric\s+video
        | visuali[sz]er
        | 4k\s+remaster(?:ed)?
        | remastered
        | hd
        | hq
    )
    \s*
    [\)\]\}]
    \s*$
    """,
    re.IGNORECASE | re.VERBOSE,
)

_YOUTUBE_ID_RE = re.compile(r"\s*\[[A-Za-z0-9_-]{11}\]\s*$")

_PLAYLIST_DIR_RE = re.compile(
    r"\s*\[(?:PL|OLAK5uy_|RD|UU|FL|LL|WL)[A-Za-z0-9_-]*\]\s*$",
    re.IGNORECASE,
)

_TRACK_PREFIX_RE = re.compile(r"^\s*(\d{1,3})\s*(?:-\s+|\.\s+)")

_AUDIO_SUFFIX_RE = re.compile(
    r"(?:\.converted)?\.(?:aac|flac|m4a|mka|mp3|oga|ogg|opus|wav|webm)$",
    re.IGNORECASE,
)


def _as_text(value: Any) -> str:
    if value is None:
        return ""
    return str(value).strip()


def _decode_path(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, bytes):
        return os.fsdecode(value)
    return str(value)


def _clean(value: Any, *patterns: re.Pattern[str]) -> str:
    """Strip each trailing pattern repeatedly, preserving non-empty text."""
    original = _as_text(value)
    cleaned = original
    while cleaned:
        for pattern in patterns:
            updated = pattern.sub("", cleaned).strip()
            if updated != cleaned:
                cleaned = updated
                break
        else:
            break
    return cleaned or original


def _strip_artist_prefix(title: str, artist: str) -> str:
    if not title or not artist:
        return title
    prefix = re.compile(rf"^{re.escape(artist)}\s+-\s+", re.IGNORECASE)
    cleaned = prefix.sub("", title, count=1).strip()
    return cleaned or title


def _playlist_album(path: str) -> str:
    directory = os.path.basename(os.path.dirname(path)) if path else ""
    if not directory or not _PLAYLIST_DIR_RE.search(directory):
        return ""
    return _PLAYLIST_DIR_RE.sub("", directory).strip()


def _track_from_path(path: str) -> int:
    match = _TRACK_PREFIX_RE.match(os.path.basename(path)) if path else None
    return int(match.group(1)) if match else 0


def _looks_like_filename(title: str, path: str) -> bool:
    """Treat an empty, extension-bearing, filename stem, or track-prefixed
    title as untagged."""
    if not title:
        return True
    if _AUDIO_SUFFIX_RE.search(title):
        return True
    name = os.path.basename(path) if path else ""
    if not name:
        return False
    stem = _AUDIO_SUFFIX_RE.sub("", name).strip()
    return title == name or title == stem or bool(_TRACK_PREFIX_RE.match(title))


def _filename_metadata(path: str) -> tuple[str, str, int]:
    """Derive artist, title, and track from a download-style filename."""
    name = os.path.basename(path) if path else ""
    if not name:
        return "", "", 0

    stem = _AUDIO_SUFFIX_RE.sub("", name).strip()
    stem = _clean(stem, _TOPIC_RE, _NOISE_RE, _YOUTUBE_ID_RE)

    track = 0
    prefix = _TRACK_PREFIX_RE.match(stem)
    if prefix:
        track = int(prefix.group(1))
        stem = stem[prefix.end() :].strip()

    stem = _clean(stem, _TOPIC_RE, _NOISE_RE, _YOUTUBE_ID_RE)
    artist = ""
    title = stem

    if " - " in stem:
        candidate_artist, candidate_title = stem.split(" - ", 1)
        candidate_artist = candidate_artist.strip()
        candidate_title = candidate_title.strip()
        # A purely numeric prefix is a track number, not an artist.
        if candidate_artist and candidate_title and not candidate_artist.isdigit():
            artist = _clean(candidate_artist, _TOPIC_RE)
            title = candidate_title

    return artist, title, track


def _item_path(item: Any, index: int, paths: list[Any], task: Any) -> str:
    path = _decode_path(getattr(item, "path", ""))
    if path:
        return path
    if index < len(paths):
        return _decode_path(paths[index])
    if len(paths) == 1:
        return _decode_path(paths[0])
    return _decode_path(getattr(task, "toppath", ""))


def _current_track(item: Any) -> int:
    try:
        return int(getattr(item, "track", 0) or 0)
    except (TypeError, ValueError):
        return 0


class YouTubeCleanPlugin(BeetsPlugin):
    def __init__(self) -> None:
        super().__init__()
        self.register_listener("import_task_start", self.clean_task)

    def clean_task(self, task: Any, session: Any = None) -> None:
        del session
        try:
            items = list(task.items)
        except (AttributeError, TypeError):
            items = []

        if not items:
            item = getattr(task, "item", None)
            items = [item] if item is not None else []

        if not items:
            return

        try:
            paths = list(task.paths)
        except (AttributeError, TypeError):
            paths = []

        changed = False
        for index, item in enumerate(items):
            path = _item_path(item, index, paths, task)

            artist = _clean(getattr(item, "artist", ""), _TOPIC_RE)
            albumartist = _clean(getattr(item, "albumartist", ""), _TOPIC_RE)
            album = _clean(getattr(item, "album", ""), _TOPIC_RE, _NOISE_RE)
            title = _clean(
                getattr(item, "title", ""), _TOPIC_RE, _NOISE_RE, _YOUTUBE_ID_RE
            )

            track = _current_track(item)
            if not title or not artist or _looks_like_filename(title, path):
                file_artist, file_title, file_track = _filename_metadata(path)
                if not artist:
                    artist = file_artist
                if _looks_like_filename(title, path):
                    title = file_title or title
                if track == 0:
                    track = file_track

            if not artist:
                artist = "Unknown Artist"

            title = _strip_artist_prefix(title, artist)

            inferred_album = False
            if album == "":
                playlist_album = _playlist_album(path)
                if playlist_album:
                    album = playlist_album
                    inferred_album = True

            if inferred_album:
                # A playlist can mix artists; keep every track in one album
                # directory instead of splitting it per track artist.
                albumartist = "Various Artists"
            elif album and not albumartist:
                albumartist = artist or "Unknown Artist"

            if track == 0:
                track = _track_from_path(path)

            fields = {
                "artist": artist,
                "albumartist": albumartist,
                "album": album,
                "title": title,
            }
            for field, value in fields.items():
                if getattr(item, field, "") != value:
                    setattr(item, field, value)
                    changed = True

            if track and track != _current_track(item):
                setattr(item, "track", track)
                changed = True

        if changed:
            # Beets caches the pre-clean source on the task, and the
            # MusicBrainz lookup runs after this event.
            task.__dict__.pop("source", None)
