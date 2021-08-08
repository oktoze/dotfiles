#!/bin/sh

setxkbmap us

SCREENSHOT_PATH=~/.tmp/i3lock.png

scrot $SCREENSHOT_PATH --quality 100
convert $SCREENSHOT_PATH -scale 4% -resize 2500% $SCREENSHOT_PATH
i3lock -i $SCREENSHOT_PATH
rm $SCREENSHOT_PATH

setxkbmap us,ir -option grp:ctrls_toggle
xmodmap ~/.Xmodmap
