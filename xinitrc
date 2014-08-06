# Load Xresources file
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# NOT WORKING ATM
# Cyberpower UPS daemon
#/usr/sbin/pwrstatd

# Run conky
#/home/smed/Downloads/conky/conky_start.sh

#modprobe vboxdrv

#xset +fp /usr/share/fonts/local
#xset fp rehash

# Enable numlock on startup
numlockx &

# Exec pulseaudio 
#/usr/bin/start-pulseaudio-x11

exec i3
#exec startxfce4

# automount usb
# udiskie -s &
udiskie --tray &

