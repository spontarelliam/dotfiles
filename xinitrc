# Load Xresources file
#[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Cyberpower UPS daemon
/usr/sbin/pwrstatd

# Run conky
#/home/smed/Downloads/conky/conky_start.sh

#modprobe vboxdrv

#xset +fp /usr/share/fonts/local
#xset fp rehash

# Enable numlock on startup
numlockx &

# Exec pulseaudio 
/usr/bin/start-pulseaudio-x11

#exec gnome-session-cinnamon
exec i3
