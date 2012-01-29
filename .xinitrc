# disable PC speaker
xset -b
# set mouse pointer
xsetroot -cursor_name left_ptr
# set keyboard layout
setxkbmap -layout 'us,ru' -option 'grp:caps_toggle'
# set random wallpaper
exec ./bin/set-wallpaper.sh &

# start applications
xcompmgr -n &
urxvtd -q -f -o &
dropboxd &

# start window manager supplied as argument by cdm
wm_bin="$1"
exec ck-launch-session bash -c "devmon & ${wm_bin}"

