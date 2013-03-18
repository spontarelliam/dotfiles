#exec gnome-session-cinnamon
exec i3

# Load Xresources file
[[ -f ~/.Xresources ]] && xrdb ~/.Xresources

# Cyberpower UPS daemon
/usr/sbin/pwrstatd

# Run conky
/home/smed/Downloads/conky/conky_start.sh

# Enable tiling of windows with ctrl+alt+keypad
quicktile.py -d

modprobe vboxdrv

xset +fp /usr/share/fonts/local
xset fp rehash
