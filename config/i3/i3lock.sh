#!/bin/sh

setxkbmap us

SCREENSHOT_PATH=~/.tmp/i3lock.png

maim $SCREENSHOT_PATH
convert $SCREENSHOT_PATH -scale 5% -resize 2000% $SCREENSHOT_PATH
i3lock -i $SCREENSHOT_PATH
rm $SCREENSHOT_PATH

setxkbmap us,ir -option grp:ctrls_toggle
xmodmap ~/.Xmodmap
