# Load Xresources file
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# NOT WORKING ATM
# Cyberpower UPS daemon
#/usr/sbin/pwrstatd

#xset +fp /usr/share/fonts/local
#xset fp rehash

# Enable numlock on startup
numlockx &

# Exec pulseaudio 
#/usr/bin/start-pulseaudio-x11

# run xrandr script
bash ~/.i3/detect_displays.sh

#exec gnome-session-cinnamon
exec i3
#exec startxfce4
#exec openbox-session

# automount usb
# udiskie -s &
udiskie --tray &


