#!/bin/sh

# sets random walpaper found in ~/.wallpaper
find ~/.wallpaper -type f \( -name '*.jpg' -o -name '*.png' \) -print0 | shuf -n1 -z | xargs -0 feh --bg-scale
