MENU="$(rofi -no-lazy-grab -sep "|" -dmenu -i -p 'System :' \
-bw 0 \
-lines 4 \
-width 15 \
-xoffset 0 -yoffset 25 \
-location 3 \
-columns 1 \
<<< "  Lock|  Logout|  Reboot|  Shutdown")"
case "$MENU" in
  *Lock) i3lock-fancy -p -t "";;
  *Logout) openbox --exit;;
  *Reboot) systemctl reboot ;;
  *Shutdown) systemctl -i poweroff
esac
