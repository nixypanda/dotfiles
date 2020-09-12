#!/home/sherub/.nix-profile/bin/zsh

STEP=0.05
MONITOR=$(xrandr -q | grep " connected" | cut -d ' ' -f1)
CurrBright=$(xrandr --verbose --current | grep ^"$MONITOR" -A5 | tail -n1)
CurrBright="${CurrBright##* }"


usage() {
    echo "Crappy custom script for brightness related operations!"
    echo ""
    echo "Usage: custom-script-backlight option"
    echo "Available options: --current/--increase/--decrease"
    echo "Only first one will be used. The script ignores everything afterwards"
}

if [ "$1" = "--current" ]; then
    NewBright=$(( CurrBright * 100))
    echo "${NewBright%.*}"
elif [ "$1" = "--increase" ]; then
    NewBright=$(( CurrBright + STEP))
    xrandr --output $MONITOR --brightness $NewBright
elif [ "$1" = "--decrease" ]; then
    NewBright=$(( CurrBright - STEP))
    xrandr --output $MONITOR --brightness $NewBright
else
    usage
fi
