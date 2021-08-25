# Dotfiles

Here lies the dotfiles crafted with much care

## Configuration

A glance at what is present in this lair.

| Name                     | What I am using (NixOS)                                                                                                                                                                       | What I am using (Mac)                                                   |
|--------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| OS                       | [NixOS](https://nixos.org/)                                                                                                                                                                   | MacOS BigSur                                                            |
| Terminal Emulator        | [Kitty](https://sw.kovidgoyal.net/kitty/)                                                                                                                                                     | [Alacritty]()                                                           |
| Shell                    | [ZSH + ohmyzsh](https://ohmyz.sh/), [Nu Shell](https://www.nushell.sh/)                                                                                                                       | [ZSH + ohmyzsh](https://ohmyz.sh/), [Nu Shell](https://www.nushell.sh/) |
| Font                     | Hack Nerd Font Mono                                                                                                                                                                           | Hack Nerd Font Mono                                                     |
| Editor                   | [Neovim](https://neovim.io/)                                                                                                                                                                  | [Neovim](https://neovim.io/)                                            |
| Desktop Environment      | [XMonad](https://xmonad.org/), [Arc-Dark](https://github.com/horst3180/arc-theme), [Polybar](https://github.com/polybar/polybar), [picom](https://github.com/yshui/picom), i3lock-fancy, rofi | Whatever comes with Mac                                                 |
| Browser                  | Google Chrome                                                                                                                                                                                 | Google Chrome                                                           |
| User environment manager | [Home Manager](https://github.com/nix-community/home-manager)                                                                                                                                 | [Home Manager](https://github.com/nix-community/home-manager)           |

## Well these look different from normal dotfiles mate

Ok so I am one of those wackoo people who are all in on the "nix hype train". That is why this
might look unfamilier to someone who does not know what nix is.

nix is a philosophy, a programing language, a package manager and an OS. All this culimates in a
unique approach to package management and system configuration. The end result is **reproducible**,
**declarative** and **reliable** systems.

Think of it like git but for your system, package management, etc `:mind_blown:`

## Color me interested! Where can I know more?

Now if you are still with me maybe you are interested to know more about it. Here are a few
helpful links that you can visit to know more about it.

- Watch first 6 minutes of [this video](https://www.youtube.com/watch?v=oPymb2-IXbg) to get a gist
  of nix package management and how it is different from standard package management and then
  building on that what is NixOS)
- If the previous video peeks your curiosity then I recommend looking at
  [this](https://www.youtube.com/watch?v=6iVXaqUfHi4) to get a deeper understanding
- Finally, if you decide that this is something you want for yourself. Then 
  [this amazing playlist](https://www.youtube.com/watch?v=QKoQ1gKJY5A&list=PL-saUBvIJzOkjAw_vOac75v-x6EzNzZq-)
  might just be the perfect place to start.

**Note: This distro of Linux is quite different from others, so please do spend some time with it.**


## Setup

Here is a walkthrough of what are the steps one need to take to get this config or parts of it
setup on any system.

### User config setup

#### Requirements

- nix with flake support is available

#### Steps

- Get contents of this repo onto your system to `~/.dotfiles`
- `cd` into `~/.dotfiles`
- Execute: `nix run home-manager --no-write-lock-file -- switch --flake  "./users#nixos"`

### Full Setup of NixOS (first time)

#### Requirements

- Some way to install NixOS
- Ability to connect to the internet
- Boot drive with label `NIXBOOT`
- Root drive where the OS will go `NIXROOT`
- Swap partition `NIXSWAP`

#### Steps

- Install NixOS
  - Make sure you have a way to get this repo. `curl`, `git`, etc
  - Make sure you enable internet
- Get contents of this repo onto your system to `~/.dotfiles`
- `cd` into `~/.dotfiles`
- Execute: `sudo nixos-rebuild switch --flake './system#nixos'`
- Execute: `nix run home-manager --no-write-lock-file -- switch --flake  "./users#nixos"`


## Code Structure

- The top level folders `system` and `users` are both flakes
- After that it is just a matter of following the code really
- In users flake `imports` is where you can see what all will end up in the final setup
  - for NixOS
  - for mac

## Here are a few screenshots to showase this config in action!

### Dracula

![Sysinfo](./screenshots/dracula/sysinfo.png?raw=true "System Info")
![Wall](./screenshots/dracula/wallpaper.png?raw=true "Wallpaper")
![App-Launcher](./screenshots/dracula/rofi-search.png?raw=true "App Launcher")
![Ricing](./screenshots/dracula/in-action.png?raw=true "Ricing in progress")
![Locked](./screenshots/dracula/locked.png?raw=true "Locked")

### Onedark

![Sysinfo](./screenshots/onedark/sysinfo.png?raw=true "System Info")
![Wall](./screenshots/onedark/wallpaper.png?raw=true "Wallpaper")
![App-Launcher](./screenshots/onedark/rofi-search.png?raw=true "App Launcher")
![Ricing](./screenshots/onedark/in-action.png?raw=true "Ricing in progress")
![Locked](./screenshots/onedark/locked.png?raw=true "Locked")
