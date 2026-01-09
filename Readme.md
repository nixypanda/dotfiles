# `dotfiles`

Here lies the `dotfiles` crafted with much care

## Configuration

A glance at what is present in this lair.

| Name                     | What I am using (Mac)                                |
| ------------------------ | ---------------------------------------------------- |
| OS                       | macOS                                                |
| Terminal Emulator        | [Kitty](https://sw.kovidgoyal.net/kitty/)            |
| Shell                    | [Nu Shell](https://www.nushell.sh/)                  |
| Font                     | Hack Nerd Font Mono                                  |
| Editor                   | [Neovim](https://neovim.io/)                         |
| Desktop Environment      | Whatever comes with Mac                              |
| Browser                  | [Firefox](https://www.mozilla.org/en-US/firefox/)    |
| User environment manager | [Home Manager](https://nixos.wiki/wiki/Home_Manager) |

## Well these look different from normal `dotfiles` mate

Ok so I am one of those people who are all in on the "nix hype train". That is
why this might look unfamiliar to someone who does not know what nix is.

Nix is a philosophy, a programming language, a package manager and an OS. All
this culminates in a unique approach to package management and system
configuration. The end result is **reproducible**, **declarative** and
**reliable** systems.

Think of it like git but for your system, package management, etc.
`:mind_blown:`

## Color me interested! Where can I know more?

Now if you are still with me and want to know more about it. Here are a few
helpful links that you can visit to know more about it.

- Watch first 6 minutes of
  [this video](https://www.youtube.com/watch?v=oPymb2-IXbg) to get a gist of nix
  package management and how it's different from standard package management and
  then building on that what is NixOS)
- If the previous video peeks your curiosity then I recommend looking at
  [this](https://www.youtube.com/watch?v=6iVXaqUfHi4) to get a deeper
  understanding
- Finally, if you decide that this is something you want for yourself. Then
  [this brilliant playlist](https://www.youtube.com/watch?v=QKoQ1gKJY5A&list=PL-saUBvIJzOkjAw_vOac75v-x6EzNzZq-)
  might just be the perfect place to start.

## Setup

Here is a walkthrough of what are the steps one need to take to get this config
or parts of it setup on any system.

### User config setup

#### Requirements

- nix with flake support is available

#### Steps

- Get contents of this repo onto your system to `~/.dotfiles`
- `cd` into `~/.dotfiles`
- Execute:
  `nix run home-manager --no-write-lock-file -- switch --flake "./#srt-l02-sekhmet"`

### Caveat

I make use of [git-crypt](https://github.com/AGWA/git-crypt) on files in the
`.secrets` folder. If you plan to use these dotfiles then you will either have
to replace those files with what you need or remove references to these files
from the codebase.

## Code Structure

- The top level folder is a flake
- After that it's just a matter of following the code really. So just open that
  `flake.nix` file and start reading it from the top.

## Here are a few screenshots to showcase this config in action

Note: These were taken on the NixOS system, I haven't been using it now. I use nix on mac now.

### Tokyonight

![System Info](./screenshots/tokyonight/sysinfo.png?raw=true "System Info")
![Widgets](./screenshots/tokyonight/widgets.png?raw=true "Wallpaper")
![App-Launcher](./screenshots/tokyonight/rofi-search.png?raw=true "App Launcher")
![Ricing](./screenshots/tokyonight/in-action.png?raw=true "Ricing in progress")
![Locked](./screenshots/tokyonight/locked.png?raw=true "Locked")

<details>
<summary>Click to see more screenshots from the past</summary>

### Dracula

![System Info](./screenshots/dracula/sysinfo.png?raw=true "System Info")
![Wall](./screenshots/dracula/wallpaper.png?raw=true "Wallpaper")
![App-Launcher](./screenshots/dracula/rofi-search.png?raw=true "App Launcher")
![Ricing](./screenshots/dracula/in-action.png?raw=true "Ricing in progress")
![Locked](./screenshots/dracula/locked.png?raw=true "Locked")

### Onedark

![System Info](./screenshots/onedark/sysinfo.png?raw=true "System Info")
![Wall](./screenshots/onedark/wallpaper.png?raw=true "Wallpaper")
![App-Launcher](./screenshots/onedark/rofi-search.png?raw=true "App Launcher")
![Ricing](./screenshots/onedark/in-action.png?raw=true "Ricing in progress")
![Locked](./screenshots/onedark/locked.png?raw=true "Locked")

</details>
