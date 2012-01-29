#!/bin/sh
amixer get Master |  awk 'FNR==6 { sub(/\[/,""); sub(/\]/,""); print $5 }'
